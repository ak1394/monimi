package
{
	import flash.events.Event;
	import flash.net.URLLoader;
	import flash.net.URLLoaderDataFormat;
	import flash.net.URLRequest;
	import flash.net.URLRequestMethod;
	import flare.data.DataSchema;
	import flare.data.DataSource;
	import flare.data.DataSet;
	import flare.data.DataTable;
	
	import flare.data.converters.Converters;
	import flash.utils.IDataInput;

	import com.adobe.serialization.json.JSON;

	public class MonimiDataSource extends DataSource
	{
        private var timezoneOffset:int;

		public function MonimiDataSource(url:String, format:String, schema:DataSchema=null)
		{
			super(url, format, schema);
            timezoneOffset = (new Date().getTimezoneOffset()) * 60 * 1000;
		}

		public function loadByName(name:String):URLLoader
		{
			var loader:URLLoader = new URLLoader();
			loader.dataFormat = URLLoaderDataFormat.BINARY;
			loader.addEventListener(Event.COMPLETE,
				function(evt:Event):void {
					loader.data = read(loader.data);
				}
			);

			var request:URLRequest = new URLRequest(this.url);
			request.data = '{"name":"' + name + '"}';
			request.contentType = "application/json";
			request.method = URLRequestMethod.POST;
			loader.load(request);
			return loader;
		}

		private function read(input:IDataInput):Object
		{
			var data:Array;
			var result:Object = parse(input.readUTFBytes(input.bytesAvailable));
			result['dataset'] = new DataSet(new DataTable(data = result['dataset'], null));
			return result;
		}

		private function parse(text:String):Object
		{
			var json:Array = JSON.decode(text.split(/\&\&/)[1]) as Array;
			var data:Array = new Array();
			var info:Object = new Object();
			info['title'] = json[0]['title'];
			info['series'] = new Array();
			trace('title');
			trace(info['title']);
			for(var series:int=1; series<json.length; series++) {
				info['series'].push(json[series][0]);
				trace("legend");
				trace(json[series][0]['legend']);
				for(var sample:int=1; sample<json[series].length; sample = sample + 2) {
					var result:Object = new Object();
					result['series'] = series;
					result['date'] = tsToDate(json[series][sample]);
					if(json[series][sample+1] == null) {
						result['failure'] = true;
						result['count'] = 0;	
					} else {
						result['count'] = json[series][sample+1] / 1000;
					}
					data.push(result);
				}	
			}
			var r:Object = new Object();
			r['info'] = info;
			r['dataset'] = data;
			return r;			
		}

		private function isoToDate(value:String):Date 
		{
			var result:Date = new Date(Date.parse(value));
            return result;
  		}

		private function tsToDate(ts:Number):Date 
		{
			var result:Date = new Date((ts * 1000) + timezoneOffset);
            return result;
  		}
	}
}
