dojo.provide("monimi.profile.Subscription");
dojo.require("dojo.date.stamp");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("monimi.profile.PaymentButton");
dojo.require("monimi.ErrorDialog");

// our declared class
dojo.declare("monimi.profile.Subscription", [dijit._Widget, dijit._Templated],
    // class properties:
    {
  	templatePath: dojo.moduleUrl("monimi.profile", "Subscription.html"),	
	widgetsInTemplate: true,
        unsubscribeUrl: null,

	postCreate: function() {
		dojo.connect(this.unsubscribeButton, 'onClick', this, 'unsubscribe');
	},
	
	start: function() {
		return monimiRPC.profilePaymentButtons().addCallback(
            dojo.hitch(this, this.onButtons)
        );
	},
	
	setProfile: function(profile) {
		this.currentPlanDiv.innerHTML = profile.plan;
		this.smsCreditsDiv.innerHTML = profile.sms_credits;
		if(profile.unsubscribe_url) {
			this.cancelDiv.style.display = 'block';
                        this.unsubscribeUrl = profile.unsubscribe_url;
		}
		
	},	

	onButtons: function(buttons) {
		dojo.forEach(buttons, function(button) {
			var hideUpgrade = true;
			var b = new monimi.profile.PaymentButton({action:button.action, fields:button.fields, name:button.name});
			if(button.kind == 'sms') {
				this.smsButtonsDiv.appendChild(b.domNode);
			} else {
				this.subsButtonsDiv.appendChild(b.domNode);
				this.upgradeDiv.style.display = 'block';
			}
		}, this);
	},	
	
	unsubscribe: function() {
    	window.location = this.unsubscribeUrl;
	}
});
