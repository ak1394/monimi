package {
	import flare.data.DataSet;
	import flare.scale.ScaleType;
	import flare.util.Orientation;
	import flare.util.palette.ColorPalette;
	import flare.vis.Visualization;
	import flare.vis.data.Data;
	import flare.vis.legend.Legend;
	import flare.vis.operator.encoder.ColorEncoder;
	import flare.vis.operator.layout.AxisLayout;
	
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.geom.Rectangle;
	import flash.net.URLLoader;
	
    [SWF(width="600", height="270", backgroundColor="#ffffff", frameRate="30")]
	public class monimicharts extends Sprite
	{
		private var dailyStats:MonimiDataSource;
		private var monitorName:String;
        private var chart:Visualization;
		
		public function monimicharts()
		{
			dailyStats = new MonimiDataSource("/controllers/monitor/daily_stats", "monimi-json");
			monitorName = this.loaderInfo.parameters.name;
			visualize(dailyStats);
		}
		
		private function visualize(stats:MonimiDataSource):void {
			var loading:Button = new Button(" Loading ...");
			loading.x = 300; 
			loading.y = 125;
			addChild(loading);			

			var loader:URLLoader =  stats.loadByName(monitorName);

            loader.addEventListener(Event.COMPLETE, function(evt:Event):void {
                var ds:DataSet = loader.data['dataset'] as DataSet;
                var info:Object = loader.data['info'];
                removeChild(loading);
                buildChart(Data.fromDataSet(ds), info);
            });
		}

        private function buildChart(data:Data, info:Object):void {
			data.createEdges("data.date", "data.series");
			chart = new Visualization(data);
			chart.bounds = new Rectangle(0, 0, 530, 180);
			var axisLayout:AxisLayout = new AxisLayout("data.date", "data.count");
			chart.operators.add(axisLayout);
			chart.operators.add(new MonimiEncoder(
				"data.count", "size", Data.NODES));
				
			chart.operators.add(new MonimiEncoder2(
				"data.count",  "fillColor", Data.NODES));
				
			chart.operators.add(new ColorEncoder(
				"data.series", Data.EDGES, "lineColor", ScaleType.CATEGORIES));

			chart.data.nodes.setProperty("alpha", 1.0);
			chart.xyAxes.yAxis.labelFormat = "0.##s";

			if(this.loaderInfo.parameters.timeFormat == "HH:mm") {
				chart.xyAxes.xAxis.labelFormat = "HH:mm";
			}
			chart.xyAxes.xAxis.showLines = false;
			chart.x = 50;			
			chart.y = 40;
			
			chart.data.edges.setProperty("lineWidth", 2);
			chart.data.edges.setProperty("lineColor", 0xff008000);
			chart.data.nodes.setProperty("lineAlpha", 0);

			var legendValues:Array = new Array();
			for(var i:int = 0; i < info.series.length; i++)
			{
				legendValues.push({color: ColorPalette.CATEGORY_COLORS_10[i], label: info.series[i].legend});
			}
			
          	var legend:Legend = Legend.fromValues(info.title, legendValues);
            legend.orientation = Orientation.LEFT_TO_RIGHT;
            legend.update();

          	legend.x = 600 - legend.width;

			chart.update();
			addChild(chart);
			addChild(legend);
        } 
	}
}
