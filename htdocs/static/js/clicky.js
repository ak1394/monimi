var clicky={init:function(){clicky.img=new Image();clicky.img_base="http://static.getclicky.com/in.php?site_id=62703&srv=db9";if(clicky.isset("clicky_custom_session")){for(var i in clicky_custom_session){clicky.img_base+="&custom["+clicky.enc(i)+"]="+clicky.enc(clicky_custom_session[i]);}}if(clicky.isset("clicky_goal")){for(var i in clicky_goal){clicky.img_base+="&goal["+clicky.enc(i)+"]="+clicky.enc(clicky_goal[i]);}}var r=RegExp("^https?://[^/]*"+location.host.replace(/^www\./i,"")+"/","i").test(document.referrer)?"":document.referrer;clicky.img.src=clicky.img_base+'&title='+clicky.enc((clicky.isset("clicky_page_title")?clicky_page_title:document.title))+'&href='+clicky.enc(location.pathname+location.search)+'&ref='+clicky.enc(r)+'&res='+screen.width+'x'+screen.height+'&lang='+(navigator.language||navigator.browserLanguage).substr(0,2)+'&x='+Math.random();clicky.add_event(window,'load',clicky.advanced);},isset:function(e){return(typeof(window[e])!="undefined");},enc:function(e){return encodeURIComponent?encodeURIComponent(e):escape(e);},add_event:function(e,type,func){if(e.addEventListener){e.addEventListener(type,func,false);}else if(e.attachEvent){e.attachEvent("on"+type,func);}},download:function(e){clicky.img_src(e,"download");},outbound:function(e){clicky.img_src(e,"outbound");},click:function(e){clicky.img_src(e,"click");},img_src:function(e,type){obj=clicky.get_target(e);clicky.log(clicky.get_href(obj),clicky.get_text(obj).substr(0,60),type);},log:function(href,title,type){type=type||"click";if(type!="outbound")href=href.replace(/^https?:\/\/([^\/]+)/i,"");clicky.img.src=clicky.img_base+"&type="+type+"&title="+clicky.enc(title)+"&href="+clicky.enc(href)+"&x="+Math.random();clicky.pause();},pause:function(){var t=clicky.isset("clicky_pause_timer")?clicky_pause_timer:500;var now=new Date();var stop=now.getTime()+t;while(now.getTime()<stop)var now=new Date();},get_text:function(e){do{var txt=e.text?e.text:e.innerText;if(txt)return txt;if(e.alt)return e.alt;if(e.title)return e.title;if(e.src)return e.src;e=clicky.get_parent(e);}while(e);return"";},get_href:function(e){do{if(e.href&&!e.src)return e.href;e=clicky.get_parent(e);}while(e);return"";},get_parent:function(e){return e.parentElement||e.parentNode;},get_target:function(e){if(!e)var e=window.event;var t=e.target?e.target:e.srcElement;if(t.nodeType&&t.nodeType==3)t=t.parentNode;return t;},advanced:function(){if(clicky.isset("clicky_advanced_disable"))return;var is_download=new RegExp("\\.(7z|aac|avi|csv|doc|exe|flv|gif|gz|jpe?g|js|m4a|mp(3|4|e?g)|mov|pdf|phps|png|ppt|rar|sit|tar|torrent|txt|wma|wmv|xls|xml|zip)$","i");var is_link=new RegExp("^(https?|ftp|telnet|mailto):","i");var is_link_internal=new RegExp("^https?:\/\/(.*)"+location.host.replace(/^www\./i,""),"i");var a=document.getElementsByTagName("a");for(var i=0;i<a.length;i++){if(a[i].className.indexOf("clicky_log")>=0){if(a[i].className.indexOf("clicky_log_download")>=0){clicky.add_event(a[i],"mousedown",clicky.download);}else if(a[i].className.indexOf("clicky_log_outbound")>=0){clicky.add_event(a[i],"mousedown",clicky.outbound);}else{clicky.add_event(a[i],"mousedown",clicky.click);}}else{if(is_link.test(a[i].href)&&a[i].className.indexOf("clicky_ignore")==-1){if(is_link_internal.test(a[i].href)){if(is_download.test(a[i].href)){clicky.add_event(a[i],"mousedown",clicky.download);}}else{clicky.add_event(a[i],"mousedown",clicky.outbound);}}}}}};clicky.init();