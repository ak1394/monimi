/*
 * File    : ping.c
 * Author  : Taavi Talvik <taavi@uninet.ee>
 * Purpose : C module for ICMP ping for erlang
 * Id      : $Id: ping.c,v 1.4 2001/09/06 22:06:39 taavi Exp $
 * Created : 24 Aug 2001 by Taavi Talvik <taavi@uninet.ee>
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>
#include <errno.h>
#include <sysexits.h>

/* ICMP includes */
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/uio.h>

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#ifndef LINUX
#include <netinet/ip_var.h>
#endif
#include <arpa/inet.h>

#include <netdb.h>

/* Erlang interface */
#include <erl_interface.h>
#include <ei.h>

/* #define	DEBUG 1 */

/* Some handy defines */
#define NULLFDS   ((struct fd_set *) 0)
#define NULLTV    ((struct timeval *) 0)
#define MAXBUFLEN 1024
#define Max(A,B) (((A) > (B)) ? (A) : (B))
#define Min(A,B) (((A) < (B)) ? (A) : (B))

/* Erlang message definitions */
#define TBHSIZE 2

/* ICMP defines */
#define MAXIPLEN        60
#define MAXICMPLEN      76
#define PHDR_LEN        sizeof(struct timeval)
#define DEFDATALEN      (64 - PHDR_LEN) /* default data length */
#define MAXPACKET       (65536 - 60 - 8)/* max packet size */



#ifdef DEBUG
# define Debug(STRING)            fprintf(stderr,STRING)
# define Debug1(STRING,Arg)       fprintf(stderr,STRING,Arg)
# define Debug2(STRING,Arg1,Arg2) fprintf(stderr,STRING,Arg1,Arg2)
#else
# define Debug(STRING)            
# define Debug1(STRING,Arg)       
# define Debug2(STRING,Arg1,Arg2) 
#endif

/* typedefs */
typedef struct PING_PARAM 
{
	char *hostname;
	int id;
	int id_len;
	int datalen;
} PING_PARAM;

typedef struct PING_RESPONSE
{
	ETERM *reply;
	int round_time;
} PING_RESPONSE;



/* global variables */
int ident;			/* process ID for identifing packets destined for us */
int icmp_socket;		/* icmp_socket */
u_char outpack[MAXPACKET];
u_char inpack[MAXPACKET];

/* prototypes */
static PING_PARAM *decode_params(char *buf);
static void do_ping( PING_PARAM *ping );
static void free_ping_param( PING_PARAM *ping);
static void pinger(PING_PARAM *ping, struct sockaddr_in whereto);
static u_short in_cksum(u_short *addr, int len);
static PING_RESPONSE *pr_pack(char *buf, int cc, struct sockaddr_in *from, struct timeval *tv);
static PING_RESPONSE *read_icmp_packet();
static void pr_icmph(struct icmp *icp);
static char *pr_addr(struct in_addr ina);
static void pr_retip(struct ip *ip);
static void tvsub(struct timeval *, struct timeval *);
void tbh_write(int fd, char buf[], int buffsize);

int main(int ac, char *av[])
{
	int stdin_fd;
	int stdout_fd;
	int sockerrno;
	int hold;

	fd_set	readfds;	/* bit field for select */
	int maxfd;		/* max fd for select 	*/
	char buf[MAXBUFLEN];	/* buffer for communication with erlang */

	PING_PARAM *ping;	/* ping parameters collected from erlang */
	PING_RESPONSE *response;/* ping response from network */
	
	/* For communication with erlang */
	stdin_fd = fileno(stdin);
	stdout_fd = fileno(stdout);

	erl_init(NULL,0);
	
	

        /*
         * Do the stuff that we need root priv's for *first*, and
         * then drop our setuid bit.  Save error reporting for
         * after arg parsing.
         */
        icmp_socket = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
        sockerrno = errno;
	setuid(getuid());
	
	if( icmp_socket < 0 ){
		errno = sockerrno;
		err(EX_OSERR, "socket");
		exit(1);
	}
	
#ifdef SO_TIMESTAMP
        { int on = 1;
        if (setsockopt(icmp_socket, SOL_SOCKET, SO_TIMESTAMP, &on, sizeof(on)) < 0)
                err(EX_OSERR, "setsockopt SO_TIMESTAMP");
        }
#endif
        /*
         * When pinging the broadcast address, you can get a lot of answers.
         * Doing something so evil is useful if you are trying to stress the
         * ethernet, or just want to fill the arp cache to get some stuff for
         * /etc/ethers.  But beware: RFC 1122 allows hosts to ignore broadcast
         * or multicast pings if they wish.
         */
        hold = 48 * 1024;
        (void)setsockopt(icmp_socket, SOL_SOCKET, SO_RCVBUF, (char *)&hold,
            sizeof(hold));

        ident = getpid() & 0xFFFF;

	
	/* Processing loop */
	while( 1 ){
		int n;
		int nr_read;
		
		
		/* set up communication with erlang */
		FD_ZERO(&readfds);
		FD_SET(stdin_fd, &readfds);
		FD_SET(icmp_socket, &readfds);
		maxfd = Max(stdin_fd,icmp_socket);

		n = select(maxfd+1, &readfds, NULLFDS,NULLFDS, NULLTV );
		if( n <= 0 ){
			perror("Error in select");
			exit(1);
		}
		if( FD_ISSET(stdin_fd, &readfds)){

			/* something to read from erlang */
			nr_read = tbh_read(stdin_fd,buf,MAXBUFLEN);

			/* Check if stdin is closed - i.e. controlling
			   process teminated */
			if (nr_read == 0 )
				exit(1);
			if( nr_read < 0 ){
				perror("Problem reading from stdin");
				exit(1);
			}
			ping = decode_params(buf+2);
			do_ping( ping );
			free_ping_param( ping );
		}
		if (FD_ISSET(icmp_socket, &readfds)) {
			int erl_reply_len;
			
			/* Read icmp response (or other icmp message, which
			   should be dropped) */
			if( !(response = read_icmp_packet( icmp_socket )) )
				continue;
			/* got response belonging to us */
			erl_reply_len = erl_encode(response->reply, buf);
			tbh_write(stdout_fd, buf, erl_reply_len);
			erl_free_term(response->reply);
			free( response );
		}
#ifdef DEBUG
		{	
			long e_alloc,e_free;
			erl_eterm_statistics(&e_alloc,&e_free);
			fprintf(stderr,"eterm: allocated=%ld,freed=%ld\r\n",e_alloc,e_free);
		}
		
#endif
	}
}

/**********************************************************************
 * Name: read_at_least(fd,buf,nr)
 * Desc: Read at least nr bytes and put them into buf. Return the number
 *       of bytes read, i.e. nr.
 * Returns: The number of bytes read, or 0 if stream closed.
 */

int read_at_least(int fd, char buf[], int nr)
{
	int remaining = nr;
	int nr_read = 0;

	while(remaining > 0)
	{
		int read_this_time;

		read_this_time = read(fd, &buf[nr_read], remaining);

		if (read_this_time == 0) /* Input stream closed? */
			return 0;

		nr_read   += read_this_time;
		remaining -= read_this_time;
	}

	return nr_read;
}

/**********************************************************************
 * Name: get_tbh_size
 * Desc: returns the size of a two_byte_header message (from Erlang).
 */

int get_tbh_size(char buf[])
{
	return (((int) buf[0]) << 8) + ((int) buf[1]);
}

/**********************************************************************
 * Name: set_tbh_size
 * Desc: sets the first two bytes of the buffer to its size
 */

void set_tbh_size(char buf[], int size)
{
	buf[1] = (char) (size & 0xff);
	buf[0] = (char) ((size >> 8) & 0xff);
	return;
}

/**********************************************************************
 * Name: tbh_write
 * Desc: writes the buffer to a file descriptor, adding size info
 *       at the beginning.
 */

void tbh_write(int fd, char buf[], int buffsize)
{
	char header_buf[TBHSIZE];

	Debug1("tbh_write: send message of size %d\r\n", buffsize);

	/* First, write two byte header */
	set_tbh_size(header_buf, buffsize);
	write(fd,header_buf,TBHSIZE);

	/* Second, write original buffer */
	write(fd,buf,buffsize);

	return;
}

/**********************************************************************
 * Name: tbh_read
 * Desc: Reads one message with two-byte-header, filling buffer.
 *       Returns the number of elements used in the buffer, or 0
 *       if the input file has been closed. 
 *
 */

int tbh_read(int fd, char buf[], int buffsize)
{
	int remaining, msgsize;

	if (read_at_least(fd,buf,TBHSIZE) != TBHSIZE) 
		return 0;

	remaining = get_tbh_size(buf);

	Debug1("tbh_read: got message of size %d\r\n",remaining);

	msgsize = read_at_least(fd, &buf[TBHSIZE],
				Min(remaining,(buffsize-TBHSIZE)));

	if (msgsize == 0)
		return 0;
	else
		return msgsize + TBHSIZE;

}

PING_PARAM *decode_params(char *buf)
{
	ETERM	*term_r;	/* erlang terms received */
	ETERM	*pattern;
	PING_PARAM *ping;
	
	
	ping = malloc( sizeof(PING_PARAM) );
	/* Interpret packets from erlang */
	/* NB! term should be freed!! */
	term_r = erl_decode(buf);

	/* prepare erlang pattern matching */
	pattern = erl_format("{ping,ParamList}");
			
	if( erl_match(pattern,term_r) ){
		/* saime {ping,[List]} */
		ETERM *param_list;
		ETERM *list;
								
		param_list = erl_var_content(pattern, "ParamList");
				
		list = param_list;
		while( ERL_IS_CONS(list) ){	/* until nonemty list */
			ETERM *param;
			param = ERL_CONS_HEAD(list);
			list = ERL_CONS_TAIL(list);
			if( ERL_IS_TUPLE(param) ){
				ETERM *atom;
				char *name;
				int len;

				atom = erl_element(1,param);
				name = ERL_ATOM_PTR(atom);
				len = ERL_ATOM_SIZE(atom);
						
				if( !strncmp(name,"address",len) ){
					ETERM *value;
							
					value = erl_element(2,param);
					ping->hostname = strdup(erl_iolist_to_string(value));
							
					erl_free_term( value );
				} else if( !strncmp(name,"id",len)) {
					ETERM *value;
					value = erl_element(2, param);
					ping->id =ERL_INT_VALUE(value);
					Debug1("id=%d\r\n",ping->id);
					erl_free_term(value);
				} else{
					fprintf(stderr,"unknown parameter passed:");
					erl_print_term(stderr,atom);
					fprintf(stderr,"\r\n");
					exit(1);
				}
				erl_free_term( atom );
			} else {
				fprintf(stderr,"unknow parameter passed:");
				erl_print_term(stderr,param);
				fprintf(stderr,"\r\n");
			}
		}
		erl_free_term( param_list );
	} else {
		fprintf(stderr, "unknown command received: ");
		erl_print_term(stderr,term_r);
		fprintf(stderr,"\r\n");
		exit( 1 );
	}
	erl_free_term( term_r );			
	erl_free_term( pattern );

	ping->datalen = DEFDATALEN;
	return( ping );
}

void do_ping( PING_PARAM *ping )
{
        struct timeval last, intvl;
        struct hostent *hp;
        struct sockaddr_in *to;
	struct sockaddr_in whereto;	/* whom to ping */
	struct sockaddr_in from;	/* source addrees? */
	char *hostname,*target, hnamebuf[MAXHOSTNAMELEN];
	char ctrl[CMSG_SPACE(sizeof(struct timeval))];

        struct iovec iov;
        struct msghdr msg;
        u_char *datap, *packet;
	
	int packlen;

        bzero(&whereto, sizeof(whereto));
        to = &whereto;
        to->sin_family = AF_INET;
#ifndef LINUX
        to->sin_len = sizeof *to;
#endif

	target = ping->hostname;
	/* Resolve name */
        if (inet_aton(target, &to->sin_addr) != 0) {
                hostname = target;
        } else {
                hp = gethostbyname2(target, AF_INET);
                if (!hp)
                        errx(EX_NOHOST, "cannot resolve %s: %s",
                             target, hstrerror(h_errno));

                if (hp->h_length > sizeof(to->sin_addr))
                        errx(1,"gethostbyname2 returned an illegal address");
                memcpy(&to->sin_addr, hp->h_addr_list[0], sizeof to->sin_addr);
                (void)strncpy(hnamebuf, hp->h_name, sizeof(hnamebuf) - 1);
                hnamebuf[sizeof(hnamebuf) - 1] = '\0';
                hostname = hnamebuf;
        }

#ifdef SO_TIMESTAMP
        { int on = 1;
        if (setsockopt(icmp_socket, SOL_SOCKET, SO_TIMESTAMP, &on, sizeof(on)) < 0)
                err(EX_OSERR, "setsockopt SO_TIMESTAMP");
        }
#endif

#ifdef DEBUG
        if (to->sin_family == AF_INET) {
                fprintf(stderr,"PING %s (%s)\r\n", hostname,
                    inet_ntoa(to->sin_addr));
        } else
                fprintf(stderr,"PING %s: %d data bytes\r\n", hostname, ping->datalen);
#endif

	/* packet */
        packlen = ping->datalen + MAXIPLEN + MAXICMPLEN;
        if (!(packet = (u_char *)malloc((size_t)packlen)))
                err(EX_UNAVAILABLE, "malloc");


        bzero(&msg, sizeof(msg));
        msg.msg_name = (caddr_t)&from;
        msg.msg_iov = &iov;
        msg.msg_iovlen = 1;
#ifdef SO_TIMESTAMP
        msg.msg_control = (caddr_t)ctrl;
#endif

	/* send one packet */
	pinger(ping, whereto);
	
}

void free_ping_param( PING_PARAM *ping)
{
	free( ping->hostname );
	free( ping );
}


/*
 * pinger --
 *      Compose and transmit an ICMP ECHO REQUEST packet.  The IP packet
 * will be added on by the kernel.  The ID field is our UNIX process ID,
 * and the sequence number is an ascending integer.  The first 8 bytes
 * of the data portion are used to hold a UNIX "timeval" struct in host
 * byte-order, to compute the round-trip time.
 */
static void
pinger(PING_PARAM *ping, struct sockaddr_in whereto)
{
        register struct icmp *icp;
        register int cc;
        int i;
	static int ntransmitted=0;
	

        icp = (struct icmp *)outpack;
        icp->icmp_type = ICMP_ECHO;
        icp->icmp_code = 0;
        icp->icmp_cksum = 0;
        icp->icmp_seq = ping->id;
        icp->icmp_id = ident;                   /* ID to know, thats our packet*/


	/* Insert time into packet */
	(void)gettimeofday((struct timeval *)&outpack[8], 
			   (struct timezone *)NULL);

        cc = ping->datalen + PHDR_LEN;                /* skips ICMP portion */

        /* compute ICMP checksum here */
        icp->icmp_cksum = in_cksum((u_short *)icp, cc);

        i = sendto(icmp_socket, (char *)outpack, cc, 0, (struct sockaddr *)&whereto,
            sizeof(whereto));

        if (i < 0 || i != cc)  {
                if (i < 0) {
                        warn("sendto");
                } else {
                        warn("%s: partial write: %d of %d bytes",
                             ping->hostname, i, cc);
                }
        }
}

/*
 * in_cksum --
 *      Checksum routine for Internet Protocol family headers (C Version)
 */
u_short
in_cksum(addr, len)
        u_short *addr;
        int len;
{
        register int nleft = len;
        register u_short *w = addr;
        register int sum = 0;
        union {
                u_short us;
                u_char  uc[2];
        } last;
        u_short answer;

        /*
         * Our algorithm is simple, using a 32 bit accumulator (sum), we add
         * sequential 16 bit words to it, and at the end, fold back all the
         * carry bits from the top 16 bits into the lower 16 bits.
         */
        while (nleft > 1)  {
                sum += *w++;
                nleft -= 2;
        }

        /* mop up an odd byte, if necessary */
        if (nleft == 1) {
                last.uc[0] = *(u_char *)w;
                last.uc[1] = 0;
                sum += last.us;
        }

        /* add back carry outs from top 16 bits to low 16 bits */
        sum = (sum >> 16) + (sum & 0xffff);     /* add hi 16 to low 16 */
        sum += (sum >> 16);                     /* add carry */
        answer = ~sum;                          /* truncate to 16 bits */
        return(answer);
}

/*
 * pr_pack --
 *      Process the packet, if it came from us.  This logic is necessary
 * because ALL readers of the ICMP socket get a copy of ALL ICMP packets
 * which arrive ('tis only fair).  This permits multiple copies of this
 * program to be run without having intermingled output (or statistics!).
 */
static PING_RESPONSE *pr_pack(char *buf, int cc, struct sockaddr_in *from, struct timeval *tv)
{
	register struct icmp *icp;
	register u_long l;
	register int i, j;
	register u_char *cp,*dp;
	static int old_rrlen;
	static char old_rr[MAX_IPOPTLEN];
	struct ip *ip;
	struct timeval *tp,tv1;
	double triptime;
	int hlen, dupflag;
	PING_RESPONSE *response;

	/* Check the IP header */
	ip = (struct ip *)buf;
	hlen = ip->ip_hl << 2;
	if (cc < hlen + ICMP_MINLEN) {
		warn("packet too short (%d bytes) from %s", cc,
		     inet_ntoa(from->sin_addr));
		return NULL;
	}

	/* Now the ICMP part */
	cc -= hlen;
	icp = (struct icmp *)(buf + hlen);
	if (icp->icmp_type == ICMP_ECHOREPLY) {
		if (icp->icmp_id != ident){
			return NULL;			/* 'Twas not our ECHO */
		}

		response = malloc(sizeof(PING_RESPONSE));

		triptime = 0.0;
#ifndef icmp_data
		tp = (struct timeval *)&icp->icmp_ip;
#else
		tp = (struct timeval *)icp->icmp_data;
#endif
		/* Avoid unaligned data: */
		memcpy(&tv1,tp,sizeof(tv1));
		tvsub(tv, &tv1);
		triptime = ((double)tv->tv_sec) * 1000.0 +
			((double)tv->tv_usec) / 1000.0;

		
		response->reply = erl_format("{ping_response,[{id,~i},{roundtrip,~f},{ttl,~i}]}",
					     (int)icp->icmp_seq,
					     triptime, (int)ip->ip_ttl);
#ifdef DEBUG
		(void)fprintf(stderr, "%d bytes from %s: icmp_seq=%u", cc,
			      inet_ntoa(*(struct in_addr *)&from->sin_addr.s_addr),
			      icp->icmp_seq);
		(void)fprintf(stderr, " ttl=%d", ip->ip_ttl);
		(void)fprintf(stderr, " time=%.3f ms", triptime);
#endif
		return response;
	} else {
		/*
		 * We've got something other than an ECHOREPLY.
		 * See if it's a reply to something that we sent.
		 * We can compare IP destination, protocol,
		 * and ICMP type and ID.
		 *
		 * Only print all the error messages if we are running
		 * as root to avoid leaking information not normally 
		 * available to those not running as root.
		 */
#ifndef icmp_data
		struct ip *oip = &icp->icmp_ip;
#else
		struct ip *oip = (struct ip *)icp->icmp_data;
#endif
		struct icmp *oicmp = (struct icmp *)(oip + 1);

		if ((oip->ip_p == IPPROTO_ICMP) &&
		    (oicmp->icmp_type == ICMP_ECHO) &&
		    (oicmp->icmp_id == ident)) {
			/* We have to handle someway other packets
			 * sent to us. Now just print some info about
			 * packet to stderr and print icmp header 
			 */
			return NULL;
		} else
		    return NULL;
	}

}

/*
 * read icmp packet from socket
 */
static PING_RESPONSE *read_icmp_packet()
{
	char ctrl[CMSG_SPACE(sizeof(struct timeval))];
	struct msghdr msg;
	struct timeval now;
	int cc;
	struct timeval *t = 0;
	struct sockaddr_in from;
	struct iovec iov;
	PING_RESPONSE *response;

#ifdef SO_TIMESTAMP
	struct cmsghdr *cmsg = (struct cmsghdr *)&ctrl;
#endif

	/* allocate packet buffer */

	bzero(&msg,sizeof(msg));
	msg.msg_name = (caddr_t)&from;
	msg.msg_iov = &iov;
	msg.msg_iovlen = 1;
	iov.iov_base = inpack;
	iov.iov_len = MAXPACKET;
#ifdef SO_TIMESTAMP
	msg.msg_controllen = sizeof(ctrl);
#endif
	msg.msg_namelen = sizeof(from);

	(void)gettimeofday(&now,NULL);
	if ((cc = recvmsg(icmp_socket, &msg, 0)) < 0) {
		if (errno == EINTR)
			return NULL;
		warn("recvmsg");
		return NULL;
	}
#ifdef SO_TIMESTAMP
	if (cmsg->cmsg_level == SOL_SOCKET &&
	    cmsg->cmsg_type == SCM_TIMESTAMP &&
	    cmsg->cmsg_len == CMSG_LEN(sizeof *t)) {
		/* Copy to avoid alignment problems: */
		memcpy(&now,CMSG_DATA(cmsg),sizeof(now));
		t = &now;
	}
#endif
	if (t == 0) {
		(void)gettimeofday(&now, NULL);
		t = &now;
	}

	response = pr_pack((char *)&inpack, cc, &from, t);
	return response;
}

/*
 * tvsub --
 *      Subtract 2 timeval structs:  out = out - in.  Out is assumed to
 * be >= in.
 */
static void
tvsub(out, in)
        register struct timeval *out, *in;
{
        if ((out->tv_usec -= in->tv_usec) < 0) {
                --out->tv_sec;
                out->tv_usec += 1000000;
        }
        out->tv_sec -= in->tv_sec;
}
