dojo.provide("monimi.ErrorDialog");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.Dialog");
dojo.require("dijit.form.Button");

// our declared class
dojo.declare("monimi.ErrorDialog", [dijit._Widget, dijit._Templated],
    // class properties:
    {
	templateString: "<div dojoAttachPoint='content' style='width: 40em; height: 10em;'><div dojoAttachPoint='messageDiv'></div><div style='padding: 1em'><button dojoType='dijit.form.Button' dojoAttachPoint='okButton'>Close</button></div></div>",
	widgetsInTemplate: true,
	title: "Error",

	postCreate: function() {
	},
	
	show: function() {
        console.log(this.messageDiv);
		this.dialog = new dijit.Dialog({title: this.title});
        this.messageDiv.innerHTML = this.message;
		this.dialog.setContent(this.content);
		this.dialog.show();
		dojo.connect(this.okButton, 'onClick', this, 'onClose');
		dojo.connect(this.dialog.closeButtonNode, 'onclick', this, 'onClose');
	},
	
	onClose: function() {
		this.dialog.destroy();
		this.destroyRecursive();	
	}
});
