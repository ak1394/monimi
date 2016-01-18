dojo.provide("monimi.Registration");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Form");	
dojo.require("dijit.form.ValidationTextBox");
dojo.require("dijit.form.CheckBox");
dojo.require("dijit.form.Button");	
dojo.require("monimi.ErrorDialog");	
dojo.require("monimi.TimezoneSelector");
dojo.require("dojox.form.BusyButton");
dojo.require("monimi.util")	

// our declared class
dojo.declare("monimi.Registration", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi", "Registration.html"),	
	widgetsInTemplate: true,

	postCreate: function() {
		dojo.connect(this.registerButton, 'onClick', this, 'register');
		dojo.connect(this.verifyButton, 'onClick', this, 'verify');
		dojo.connect(this.activateButton, 'onClick', this, 'confirm');
		dojo.connect(this.alreadyGot, 'onclick', this, 'showVerify');
		dojo.connect(this.smsStatusRefreshHref, 'onclick', this, 'refreshSmsStatus');
		
		Recaptcha.create("6LeU9AAAAAAAAM7atpS2eFSuAtf4M4AFU2QyHkM3",
			"recaptcha_div", {
		   		theme: "white",
				tabindex: 5
			});

		this.registration_fields = {
			email: {name: 'Email', textbox: this.formEmail},
			password: {name: 'Password', textbox: this.formPassword},
            confirm: {name: 'Confirm password', textbox: this.formPasswordConfirm},
			mobile: {name: 'Mobile', textbox: this.formMobile},
		    captcha_challenge: {value: Recaptcha.get_challenge},
			captcha_response: {name: 'Captcha', value: Recaptcha.get_response, domNode: function(){ return dojo.byId("recaptcha_area") }}
		};

        this.confirmation_fields = {
			confirmation_code: {name: 'Confirmation Code', textbox: this.formCode},
			timezone: {
				name: 'Time Zone',
				value: dojo.hitch(this.timeZone, "getZone"),
				domNode: dojo.hitch(this, function(){
					return this.timeZone.domNode
				})
			}
        };

		this.formEmail.focus();
		
		monimiRPC.registrationGetLocation().addCallbacks(
            dojo.hitch(this, function(result) {
                this.registrationDiv.style.display = 'block';
				if (result.calling_code) {
					this.formMobile.attr('value', '+' + result.calling_code);
				}
				if (result.time_zone) {
					this.timeZone.setZone(result.time_zone);
				} else {
					this.timeZone.setZone();
				}
            }), 
            dojo.hitch(this, 'onRpcError')
        );
	},
	
	register: function() {
		var params = monimi.util.form_params(this.registration_fields);
        this.registerButton.makeBusy();
		monimiRPC.registrationCreateUser(params).addCallbacks(
			dojo.hitch(this, 'onRegister'), dojo.hitch(this, 'onRegisterError'));
	},
	
	onRegister: function(result) {
		this.sms_id = result.sms_id;
	    this.showVerify();
		console.log(this);
	},

	onRegisterError: function(error) {
 		this.registerButton.cancel();			
		if(error.message == 'validation_failed') {
            Recaptcha.reload();
			monimi.util.form_display_error(this.registration_fields, error.errorObject.detail);			
		} else {
            this.onRpcError(error);
        }
	},

	showVerify: function() {
		this.registrationDiv.style.display = 'none';	
		this.verifyDiv.style.display = 'block';	
	},
	
	verify: function() {
		this.showConfirm();	
	},

	showConfirm: function() {
		this.verifyDiv.style.display = 'none';
		this.confirmationDiv.style.display = 'block';	
		this.formCode.focus();
		if(this.sms_id) {
			this.refreshSmsStatus();
			this.smsStatusDiv.style.display = 'block';	
		}
	},

	confirm: function() {
		var params = monimi.util.form_params(this.confirmation_fields);
		monimiRPC.registrationConfirmUser(params).addCallbacks(
			dojo.hitch(this, 'onConfirmation'), dojo.hitch(this, 'onConfirmationError'));
	},

	onConfirmation: function(result) {
        window.location.href = "http://" + window.location.host + "/home";
	},

	onConfirmationError: function(error) {
 		this.activateButton.cancel();
		if(error.message == 'validation_failed') {
			monimi.util.form_display_error(this.confirmation_fields, error.errorObject.detail);			
		} else {
            this.onRpcError(error);
        }

	},
	
	refreshSmsStatus: function() {
		if(this.sms_id) {
			this.smsRefreshImg.style.opacity = 0.3;
			monimiRPC.registrationSmsStatus({sms_id: this.sms_id}).addCallbacks(
				dojo.hitch(this, 'onSmsStatus'), dojo.hitch(this, 'onRpcError'));
		}
	},

	onSmsStatus: function(result) {
		this.smsStatusSpan.innerHTML = result.status_msg;
		this.smsRefreshImg.style.opacity = 1.0;
		
	},
	
    onRpcError: function(error) {
        var dialog = new monimi.ErrorDialog({
            title: "RPC Error occured",
            message: "Error: " + error.message
            });
        dialog.show();
    }

});
