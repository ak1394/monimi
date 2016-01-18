dependencies = {
	layers: [
		{
			name: "monimi-registration.js",
			dependencies: [
				'monimi.JsonService',
				'monimi.Registration'
			]
		},
		{
			name: "monimi-password.js",
			dependencies: [
				'monimi.JsonService',
				'monimi.RecoverPassword'
			]
		},
		{
			name: "monimi.js",
			dependencies: [
				'monimi.JsonService',
				'monimi.Home'
			]
		},
		{
			name: "monimi-reports.js",
			dependencies: [
				'monimi.JsonService',
				'monimi.Reports'
			]
		},
		{
			name: "monimi-profile.js",
			dependencies: [
				'monimi.JsonService',
				'monimi.Profile'
			]
		},
		{
			name: "monimi-notifications.js",
			dependencies: [
				'monimi.monitor.Notifications'
			],
            layerDependencies: [ "monimi.js" ]
		}
	],

	prefixes: [
		[ "dijit", "../../dojo-release-1.2.3-src/dijit" ],
		[ "dojox", "../../dojo-release-1.2.3-src/dojox" ],
		[ "monimi", "../../monimi" ]
	]
}
