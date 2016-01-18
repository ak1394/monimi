dojo.provide("monimi.profile.Personal");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Form");	
dojo.require("dijit.form.TextBox");
dojo.require("monimi.TimezoneSelector");
dojo.require("dojox.form.BusyButton");
dojo.require("monimi.util")	

// our declared class
dojo.declare("monimi.profile.Personal", [dijit._Widget, dijit._Templated],
    // class properties:
    {
  	templatePath: dojo.moduleUrl("monimi.profile", "Personal.html"),	
	widgetsInTemplate: true,

	postCreate: function() {
		this.password_fields = {
			old_password: {name: 'Old Password', textbox: this.passwordOld},
			new_password: {name: 'New Password', textbox: this.passwordNew},
			confirm_password: {name: 'Confirm Password', textbox: this.passwordConfirm}
		};
		this.user_fields = {
			first_name: {name: 'First Name', textbox: this.first_name},
			last_name: {name: 'Last Name', textbox: this.last_name},
			username: {name: 'Username', textbox: this.username}
		};
		dojo.connect(this.userButton, 'onClick', this, 'changeUser');
		dojo.connect(this.passwordButton, 'onClick', this, 'changePassword');
		dojo.connect(this.zoneButton, 'onClick', this, 'changeTimezone');
	},
	
	show: function() {
		this.tabContainer.resize();
	},
	
	changeTimezone: function() {
		monimiRPC.profileChangeTimeZone({timezone:this.timeZone.getZone()}).addCallback(
			dojo.hitch(this, function() {
				this.zoneButton.cancel();
			}));			
	},
	
	changePassword: function() {
		var params = monimi.util.form_params(this.password_fields);
        this.passwordButton.makeBusy();
		monimiRPC.profileChangePassword(params).addCallbacks(
			dojo.hitch(this, 'onChangePassword'), dojo.hitch(this, 'onChangePasswordError'));
	},

	changeUser: function() {
		var params = monimi.util.form_params(this.user_fields);
        this.userButton.makeBusy();
		monimiRPC.profileChangeUser(params).addCallbacks(
			dojo.hitch(this, 'onChangeUser'), dojo.hitch(this, 'onChangeUserError'));
	},
	
	onChangePasswordError: function(error) {
 		this.passwordButton.cancel();			
		if(error.message == 'validation_failed') {
			monimi.util.form_display_error(this.password_fields, error.errorObject.detail);			
		} else {
            this.onRpcError(error);
        }
	},

	onChangePassword: function() {
 		this.passwordButton.cancel();
		this.passwordOld.reset();
		this.passwordNew.reset();
		this.passwordConfirm.reset();
	},

	onChangeUserError: function(error) {
 		this.userButton.cancel();			
		if(error.message == 'validation_failed') {
			monimi.util.form_display_error(this.user_fields, error.errorObject.detail);			
		} else {
            this.onRpcError(error);
        }
	},

	onChangeUser: function() {
		if(this.username.attr('value') != "") {
			this.username.attr('disabled', true);
		}
 		this.userButton.cancel();
	},
	
	setProfile: function(profile) {
		this.timeZone.setZone(profile.timezone);
		this.first_name.attr('value', profile.first_name);
		this.last_name.attr('value', profile.last_name);
		this.username.attr('value', profile.username);
		if(profile.username == null) {
			this.username.attr('disabled', false);
		}
		//this.mobile.setValue(profile.mobile);
	},

    onRpcError: function(error) {
        var dialog = new monimi.ErrorDialog({
            title: "RPC Error occured",
            message: "Error: " + error.message
            });
        dialog.show();
    }
});
