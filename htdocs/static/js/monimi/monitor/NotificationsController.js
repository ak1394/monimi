dojo.provide("monimi.monitor.NotificationsController");

dojo.require("dojo.date.stamp");

dojo.declare("monimi.monitor.NotificationsController", null,
    {
	constructor: function(view, monitor, parent_controller) {
		this.view = view;
		this.parent_controller = parent_controller;
		this.monitor = monitor;
		this.rpc = parent_controller.rpc;
		this.loadNotifications().addCallback(dojo.hitch(this.view, this.view.displayNotifications));
	},

	loadNotifications: function() {
		return this.rpc.monitorListNotifications({name:this.monitor.name}).addCallbacks(
			dojo.hitch(this, this.onNotifications),
			dojo.hitch(this, 'onRpcError')
		);		
	},

	onNotifications: function(notifications) {
		this.notifications = notifications;
		dojo.forEach(this.notifications, function(notification) {
			notification.qp_start = dojo.date.stamp.fromISOString(notification.qp_start);
			notification.qp_end = dojo.date.stamp.fromISOString(notification.qp_end);
		});
		return this.notifications;
	},

	setQuietPeriod: function(notification_id, qp_start, qp_end) {
		var notification = this.notifications[notification_id];
		notification.qp_start = qp_start;
		notification.qp_end = qp_end;
		this.view.updateQpButton(notification_id, notification);
		this.view.enableApplyButton();
	},

	submitChanges: function() {
		var notifications = [];
		dojo.forEach(this.notifications, function(notification, notification_id) {
			notification.notify_down_on = dojo.byId('notify_down_' + notification_id).value;
			notification.notify_up = dojo.byId('notify_up_' + notification_id).checked;
			var now = new Date();
			if(notification.qp_start) {
				var qp_start = dojo.date.stamp.toISOString(new Date(now.getFullYear(),
																now.getMonth(),
																now.getDate(),
																notification.qp_start.getHours(),
																notification.qp_start.getMinutes()
																));
			} else {
				var qp_start = null;
			}
			
			if(notification.qp_end) {
				var qp_end = dojo.date.stamp.toISOString(new Date(now.getFullYear(),
																now.getMonth(),
																now.getDate(),
																notification.qp_end.getHours(),
																notification.qp_end.getMinutes()
																)); 
			} else {
				var qp_end = null;
			}

			var _notification = {
				name: notification.name,
				notify_down_on: notification.notify_down_on,
				notify_up: notification.notify_up,
				qp_start: qp_start, 				
				qp_end: qp_end  				
			};
			notifications.push(_notification);
		});
		this.view.busyApplyButton();
		return this.rpc.monitorSetNotifications({
			name:this.monitor.name,
			notifications: notifications}).addCallback(dojo.hitch(this, function() {
				this.view.disableApplyButton()	
			}));
	},

	onRpcError: function(error) {
		this.parent_controller.onRpcError("notifications:" + error);
	}
});
