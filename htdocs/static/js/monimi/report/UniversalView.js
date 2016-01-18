dojo.provide("monimi.report.UniversalView");

dojo.require("dojo.date");
dojo.require("dojo.date.stamp");
dojo.require("dojo.date.locale");
dojo.require("dijit._Widget");
dojo.require("dijit._Container");
dojo.require("dijit._Templated");

dojo.require("monimi.Table");

// our declared class
dojo.declare("monimi.report.UniversalView", [dijit._Widget, dijit._Templated, dijit._Container], {
  	templatePath: dojo.moduleUrl("monimi.report", "UniversalView.html"),	

	postMixInProperties: function() {
		this.title = this.monitor.name + ": " + this.date;		
	},

	format_timestamp: function(name, format, timezoneOffset, cell, item) {
		if(format) {
			cell.appendChild(dojo.doc.createTextNode(dojo.date.locale.format(new Date((item[name] * 1000) + timezoneOffset), {selector:format})));
		} else {
			cell.appendChild(dojo.doc.createTextNode(dojo.date.locale.format(new Date((item[name] * 1000) + timezoneOffset))));	
		}
	},
	
	format_status: function(error_span, cell, item) {
		if(item.error) {
			dojo.addClass(cell, 'red');
			cell.appendChild(dojo.doc.createTextNode(item.error));
			cell.setAttribute('colspan', error_span + 1);
			cell.setAttribute('align', 'center');
			return error_span;
		} else {
			dojo.addClass(cell, 'green');
		}
	},

	format_node: function(name, cell, item) {
		var node_names = [null, 'Missouri, US', 'Georgia, US', 'London, UK'];
		cell.appendChild(dojo.doc.createTextNode(node_names[item[name]]));
	},

	format_duration: function(name, cell, item) {
		var downtime = new Date(item[name] * 1000);
		var formatted = dojo.string.pad(downtime.getUTCHours(), 2) + ":" + 
			dojo.string.pad(downtime.getUTCMinutes(), 2) + ":" +
			dojo.string.pad(downtime.getUTCSeconds(), 2);
		cell.appendChild(dojo.doc.createTextNode(formatted));
	},

	format_normal: function(name, unit, cell, item) {
		var value = item[name];
		if(unit) {
			value = value + " " + unit;
		}
		cell.appendChild(dojo.doc.createTextNode(value));
	},
	
	setData: function(items) {
		var titles = [];
		var names = [];
		var plan = items[0];
		var data = items[1];
		
		var normal_facets = 0;
		for (var i = 0; i < plan.length; i++) {
			if(plan[i].type == 'normal') {
				normal_facets++;	
			}
		}
		
		for(var i=0; i<plan.length;i++) {
			titles.push(plan[i].title);
			if(plan[i].format == 'normal') {
				names.push(dojo.partial(this.format_normal, plan[i].name, plan[i].unit));
			} else if(plan[i].format == 'timestamp') {
                var timezoneOffset = (new Date()).getTimezoneOffset() * 60 * 1000;
				names.push(dojo.partial(this.format_timestamp, plan[i].name, plan[i].selector, timezoneOffset));
			} else if(plan[i].format == 'error') {
				// pass number of normal facets to override
				names.push(dojo.partial(this.format_status, normal_facets));
			} else if(plan[i].format == 'node') {
				names.push(dojo.partial(this.format_node, plan[i].name));
			} else if(plan[i].format == 'duration') {
				names.push(dojo.partial(this.format_duration, plan[i].name));
			}
		}

		var table = new monimi.Table({titles:titles, names:names, items: data});

		dojo.removeClass(this.titleDiv, 'loading');		   
		this.titleDiv.innerHTML = this.title;
		this.addChild(table);
	}
});
