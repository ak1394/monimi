dojo.provide("monimi.MonitorView");

dojo.require("dojo.date.locale");
dojo.require("dojo.string");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.layout.TabContainer");
dojo.require("monimi.flash");
dojo.require("monimi.util");

dojo.declare("monimi.MonitorView", [dijit._Widget, dijit._Templated],
    {
  	templatePath: dojo.moduleUrl("monimi", "MonitorView.html"),
	widgetsInTemplate: true,
	monitor: null,
	monitorName: null,
	controller: null,
	current: null,
	wlist: [],
	initiallySelectedTab: null,

	postMixInProperties: function() {
		this.monitor = this.controller.getMonitor(this.monitorName);
	},

	getStatusHTML: function() {
		var values = {
			monitor: this.monitor,
			
			check_type: this.monitor.kind == "KIND_HTTP" ? "HTTP"
				: this.monitor.kind == "KIND_HTTPS" ? "HTTPS"
					: this.monitor.kind == "KIND_PING" ? "Ping"
						: this.monitor.kind == "KIND_DNS" ? "DNS"
							: "Unknown",
			url: this.monitor.url,
			
			status: this.monitor.user_state == 'ST_PAUSED' ? "Paused"
				: this.monitor.state == 'ST_ADDED' ? "Waiting for data"
					: this.monitor.state == 'ST_UP' ? "Up"
						: this.monitor.state == 'ST_DOWN' ? "Down"
							: "Unknown"
		};

		var templ_http = '<table class="stats">' +
				'<tr><td class="name">Status:</td><td>${status}</td></tr>' +
				'<tr><td class="name">Type:</td><td>${check_type}</td></tr>' +
				'<tr><td class="name">URL:</td><td><a href="${url}" target="_blank">${url}</a></td></tr>' +
				'</table>';

		var templ_ping = '<table class="stats">' +
				'<tr><td class="name">Status:</td><td>${status}</td></tr>' +
				'<tr><td class="name">Type:</td><td>${check_type}</td></tr>' +
				'<tr><td class="name">Hostname:</td><td>${url}</td></tr>' +
				'</table>';

		var templ_dns = '<table class="stats">' +
				'<tr><td class="name">Status:</td><td>${status}</td><td class="name">Nameserver:</td><td>${ns}</td></tr>' +
				'<tr><td class="name">Type:</td><td>${check_type}/${qtype}</td><td class="name">Hostname:</td><td>${name}</td></tr>' +
				'<tr></tr>' +
				'</table>';

		if(this.monitor.kind == 'KIND_HTTP' || this.monitor.kind == 'KIND_HTTPS') {
			return dojo.string.substitute(templ_http, values);
		} else if(this.monitor.kind == 'KIND_PING') {
			return dojo.string.substitute(templ_ping, values);
		}  else if(this.monitor.kind == 'KIND_DNS') {
			monimi.util.proplists.collapse(dojo.fromJson(this.monitor.url), values);
			if(!values.ns) {values.ns = "Any"};
			if(!values.qtype) {values.qtype = "A"};
			values.qtype = values.qtype.toUpperCase();
			return dojo.string.substitute(templ_dns, values);
		}
	},
	
	postCreate: function() {
		monimi.flash.embedSWF("/static/flash/monimicharts.swf", this.monitorChartsDiv, 600, 270, "9.0.28",
						   {name:this.monitor.name, 
						   timeFormat:dojo.date.locale._getGregorianBundle()['timeFormat-short']},
						{wmode: 'transparent'}, {wmode: 'transparent'});

		this.infoDiv.innerHTML = this.getStatusHTML();

		this.loadOnDemand(this.notificationsCP, 'monimi.monitor.Notifications', 'monimi-notifications.js', {parent_controller:this.controller, monitor:this.monitor});
		this.loadOnDemand(this.pauseCP, 'monimi.monitor.Pause', null, {controller:this.controller, monitor:this.monitor});
		this.loadOnDemand(this.removeCP, 'monimi.monitor.Remove', null, {controller:this.controller, monitor:this.monitor});
		this.loadOnDemand(this.settingsCP, 'monimi.monitor.Settings', null, {parent_controller:this.controller, monitor:this.monitor});
		
		if(this.initiallySelectedTab != null) {
			this.tabContainer.selectChild(this.tabContainer.getChildren()[this.initiallySelectedTab]);
		}

 	},
	
	loadOnDemand: function(contentPane, name, layer, params) {
		var handler = dojo.connect(contentPane, "onShow", this, function() {
			var w_class = monimi.util.require(name, layer);
			var w = new w_class(params);
			contentPane.setContent(w.domNode);
			this.wlist.push(w);
			dojo.disconnect(handler);
		});
	},
	
	getSelectedTab: function() {
		return dojo.indexOf(this.tabContainer.getChildren(), this.tabContainer.selectedChildWidget);
	},
	
	refresh: function() {
		this.monitor = this.controller.getMonitor(this.monitor.name);
		this.infoDiv.innerHTML = this.getStatusHTML();
	},
	
	destroy: function() {
		dojo.forEach(this.wlist, function(widget) {
			widget.destroy();
		});
		dojo.forEach(this.tabContainer.getChildren(), function(child) {
			child.destroyRecursive();
		});
		this.inherited(arguments);	
	}
});
