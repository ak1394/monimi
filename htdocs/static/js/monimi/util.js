dojo.provide("monimi.util");

dojo.require("dijit.Tooltip");

monimi.util = {
	form_params: function(fields) {
		var params = {};

        // clear previous errors
        for(var field_name in fields) {
			var domNode = null;
			if(fields[field_name].textbox) {
				domNode = fields[field_name].textbox.domNode;
                dojo.hitch(fields[field_name].textbox, 'validate')(true);
			} else if(fields[field_name].domNode) {
				domNode = fields[field_name].domNode();
			}
            if(domNode) {
                dojo.removeClass(domNode, 'validationError');
            }
        }
		
		for(var field_name in fields) {
			if(fields[field_name].textbox) {
				params[field_name] = dojo.hitch(fields[field_name].textbox, 'getValue')();	
			} else if (fields[field_name].widget) {
				params[field_name] = fields[field_name].widget.attr('value');
			} else if (fields[field_name].value) {
				params[field_name] = fields[field_name].value();
			}
		}
		return params;
	},
	
	form_display_error: function(fields, detail) {
        // display errorrs
		for(var field_name in detail) {
			var domNode = null;
			if(fields[field_name].textbox) {
				domNode = fields[field_name].textbox.domNode;
                dojo.hitch(fields[field_name].textbox, 'validate')(true);
			} else if(fields[field_name].domNode) {
				domNode = fields[field_name].domNode();
			}
			if (domNode) {
                dojo.addClass(domNode, 'validationError');
				dijit.showTooltip(detail[field_name], domNode);
				var link = dojo.connect(domNode, 'onmouseover', function(){
					dijit.hideTooltip(domNode);
					dojo.disconnect(link);
				});
				break;
			}		
		}
	},

	form_format_error: function(fields, detail) {
        var result = "";
		for(var field_name in detail) {
            var displayName = 
            result = result + dojo.string.substitute(
                "<li><b>${0}:</b> ${1}</li>",
                [fields[field_name].name, detail[field_name]]);
		}
        return "Invalid parameters:<ul>" + result + "</ul>";
	},
	
	form_log_error: function(fields, detail) {
		for(var field_name in detail) {
			console.log("error", fields[field_name].name, detail[field_name]); 
		}
	},
	
	
	require: function(modulename, layer) {
			if(layer) {
				dojo._loadUri('/static/js/dojo/dojo/' + layer);
			}
			dojo['require'](modulename);
			var syms = modulename.split(".");
			var result = window;
			for(var i = 0; i<syms.length; i++){
				result = result[syms[i]];
			}
			return result;
	},

	datesDiff: function(date_earlier, date_later) {
		if(date_earlier == null || date_later == null) {
			return null;
		}
		var diff = date_later - date_earlier;
		var milliseconds = Math.floor(diff % 1000);   
		    diff = diff/1000;            
		var seconds = Math.floor(diff % 60);
		    diff = diff/60;
		var minutes = Math.floor(diff % 60);
		    diff = diff/60;
		var hours = Math.floor(diff % 24);
		    diff = diff/24;
		var days = Math.floor(diff);
		return {ms:milliseconds, seconds:seconds, minutes:minutes, hours:hours, days:days};
	},
	
	proplists: {
		collapse: function(list, result) {
			if(!result) {
				var result = {};
			}
			dojo.forEach(list, function(element) {
				for(key in element) {
					result[key] = element[key];
				}
			});
			return result;
		},
		
		get: function(key, list) {
			dojo.forEach(list, function(element) {
				if(element[key]) {
					return element[key];
				}
			});
		}
	}	
}
