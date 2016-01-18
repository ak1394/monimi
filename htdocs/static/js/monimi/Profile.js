dojo.provide("monimi.Profile");
dojo.require("dojo.date.stamp");
dojo.require("dojo.DeferredList");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("monimi.profile.Subscription");
dojo.require("monimi.profile.Personal");
dojo.require("monimi.ErrorDialog");

// our declared class
dojo.declare("monimi.Profile", [dijit._Widget, dijit._Templated],
    // class properties:
    {
  	templatePath: dojo.moduleUrl("monimi", "Profile.html"),	
	widgetsInTemplate: true,
    profile: null,
    payment_buttons: null,

	postCreate: function() {
		var d1 = monimiRPC.profileGetProfile().addCallbacks(
			dojo.hitch(this, function(profile) {this.profile = profile;}),
			dojo.hitch(this, 'onRpcError')
		);	

        (new dojo.DeferredList([d1, this.subscription.start()])).addCallbacks(dojo.hitch(this, function() {
		        this.personal.setProfile(this.profile);
		        this.subscription.setProfile(this.profile);
				this.domNode.style.display = 'block';
				this.personal.show()
            }), dojo.hitch(this, this.onRpcError));
	},

	onRpcError: function(error) {
		var dialog = new monimi.ErrorDialog({
			title: "RPC Error occured",
			message: "Error: " + error.message
			});
        dialog.show();
	}
});
