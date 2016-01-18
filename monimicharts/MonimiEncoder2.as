package
{
	import flare.vis.operator.encoder.Encoder;
	import flare.vis.data.Data;
	
	public class MonimiEncoder2 extends Encoder
	{
		
		public function MonimiEncoder2(source:String=null, target:String=null,
							group:String=Data.NODES, filter:*=null)
		{
			super(source, target, group, filter);
		}

		/** @inheritDoc */
		protected override function encode(val:Object):*
		{
			return 0xffef0000;
		}
	}
}