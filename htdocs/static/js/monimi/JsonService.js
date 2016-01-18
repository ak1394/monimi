dojo.provide("monimi.JsonService");
dojo.require("dojo.rpc.RpcService");

dojo.declare("monimi.JsonService", dojo.rpc.RpcService, {
	strictArgChecks: false,
	bind: function(method, parameters, deferredRequestHandler, url){
		//summary
		var def = dojo.rawXhrPost({
			url: url,
			postData: this.createRequest(parameters),
			contentType: "application/json",
			timeout: this.timeout, 
			handleAs: "json-comment-optional"
		});
		
		def.addCallbacks(this.resultCallback(deferredRequestHandler), this.errorCallback(deferredRequestHandler));
	},
	
	parseResults: function(obj) {
		return obj;	
	},
	
	createRequest: function(parameters){
		var params = (dojo.isArrayLike(parameters) && parameters.length==1) ?
				parameters[0] : {};
		return dojo.toJson(params);				
	}
	
});