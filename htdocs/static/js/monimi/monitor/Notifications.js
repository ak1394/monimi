dojo.provide("monimi.monitor.Notifications");

dojo.require("dojo.date.locale");
dojo.require("dijit._Widget");
dojo.require("dijit.form.Button");
dojo.require("dijit.form.CheckBox");
dojo.require("dijit.layout.ContentPane");
dojo.require("dojox.dtl._Templated");
dojo.require("dojox.form.BusyButton");
dojo.require("monimi.monitor.NotificationsController");
dojo.require("monimi.monitor.QuietPeriodDialog");

dojo.declare("monimi.monitor.Notifications", [dijit._Widget, dojox.dtl._Templated],
    {
    templatePath: dojo.moduleUrl("monimi.monitor", "Notifications.html"),	
	widgetsInTemplate: true,
	monitor: null,
	controller: null,
						
    postCreate: function() {
		this.controller = new monimi.monitor.NotificationsController(this, this.monitor, this.parent_controller);
	},
	
	_formatDisplayPeriod: function(qp_start, qp_end) {
		if(qp_start) {
			return dojo.date.locale.format(qp_start, {selector:'time'}) + 
						' - ' + dojo.date.locale.format(qp_end, {selector:'time'});
		}
		return 'None';
	},
	
	displayNotifications: function(notifications) {
		this.notifications = notifications;
		dojo.forEach(this.notifications, function(notification) {
			notification.display_period = this._formatDisplayPeriod(
				notification.qp_start, notification.qp_end);
		}, this); 
		this.render();
		dojo.forEach(this.notifications, function(notification, notification_id) {
			this.connect(dojo.byId('notify_down_' + notification_id), 'onchange', this.enableApplyButton);
			this.connect(dijit.byId('notify_up_' + notification_id), 'onChange', this.enableApplyButton);
			this.connect(dijit.byId('button_change_qp_' + notification_id), 'onClick', function() {
				this.changeQuietPeriod(notification_id);
			});
		}, this);
		this.disableApplyButton();
		dojo.connect(this.applyButton, "onClick", this.controller, 'submitChanges');
	},
	
	enableApplyButton: function() {
		this.applyButton.cancel();
	},

	disableApplyButton: function() {
		this.applyButton.cancel();
		this.applyButton.attr('disabled', true);
	},

	busyApplyButton: function() {
		this.applyButton.makeBusy();
	},
	
	updateQpButton: function(notification_id, notification) {
		notification.display_period = this._formatDisplayPeriod(
			notification.qp_start, notification.qp_end);
		dijit.byId('button_change_qp_' + notification_id).setLabel(notification.display_period);
	},
	
	changeQuietPeriod: function(notification_id) {
		new monimi.monitor.QuietPeriodDialog({
			notification_id: notification_id,
			controller:this.controller,
			notification:this.notifications[notification_id]
		}).show();		
	},
	
	onClick: function(a) {
	},
		
	destroy: function() {
		this.contentPane.destroyRecursive();
		this.inherited(arguments);
	}
});
