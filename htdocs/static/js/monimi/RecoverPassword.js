dojo.provide("monimi.RecoverPassword");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Form");	
dojo.require("dijit.form.ValidationTextBox");
dojo.require("dijit.form.Button");	
dojo.require("monimi.ErrorDialog");	
dojo.require("monimi.util")	

// our declared class
dojo.declare("monimi.RecoverPassword", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi", "RecoverPassword.html"),	
	widgetsInTemplate: true,

	postCreate: function() {
		dojo.connect(this.submitButton, 'onClick', this, 'submit');
		dojo.connect(this.confirmButton, 'onClick', this, 'confirm');

		Recaptcha.create("6LeU9AAAAAAAAM7atpS2eFSuAtf4M4AFU2QyHkM3",
			"recaptcha_div", {
		   		theme: "white",
				tabindex: 5
			});
		this.formEmail.focus();
		this.fields = {
			email: {name: 'Email', textbox: this.formEmail},
		    captcha_challenge: {value: Recaptcha.get_challenge},
			captcha_response: {name: 'Captcha', value: Recaptcha.get_response, domNode: function(){ return dojo.byId("recaptcha_area") }}
		};
	},
	
	submit: function() {
		var params = monimi.util.form_params(this.fields);
		monimiRPC.recoverpasswordRecoverPassword(params).addCallbacks(
			dojo.hitch(this, 'onSuccess'), dojo.hitch(this, 'onError'));
	},

	confirm: function() 
	{
		window.location.href = window.location.protocol + "//" + window.location.host + "/";
	},

	onSuccess: function(result) {
		this.registrationDiv.style.display = 'none';	
		this.confirmationDiv.style.display = 'block';	
	},

	onError: function(error) {
		if(error.message == 'validation_failed') {
            Recaptcha.reload();
			console.log("err", this.fields, error.errorObject.detail);
			monimi.util.form_display_error(this.fields, error.errorObject.detail);			
		} else {
			(new monimi.ErrorDialog({title: "RPC Error occured", message: "Error: " + error.message})).show();
		}
	}
});
