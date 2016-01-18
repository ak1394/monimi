package
{
	import flare.vis.operator.encoder.Encoder;
	import flare.vis.data.Data;
	
	public class MonimiEncoder extends Encoder
	{
		
		public function MonimiEncoder(source:String=null, target:String=null,
							group:String=Data.NODES, filter:*=null)
		{
			super(source, target, group, filter);
		}

		/** @inheritDoc */
		protected override function encode(val:Object):*
		{
			if(val.failure) {
				return 0.5;	
			} else {
				return 0.0;
			}
		}
	}
}