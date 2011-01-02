package com.turbostool.client.model {
import com.turbostool.client.utils.Utils;

[Bindable]
public class WearOutUpgrade {
	public function WearOutUpgrade() {
	}

	public var id: int;
	public var displayName: String;
	public var delta: Number;
	public var durability: Number;
  public var durabilityMax: Number;

	public function toString():String {
		return	displayName + " - " + Utils.numberToString(delta * 100, 2, 1) + "% (" + Utils.numberToString(durability * 100, 2, 1) + "%)";
	}

	public static function create(id:int, displayName:String, delta:Number, durability:Number, durabilityMax:Number): WearOutUpgrade {
		var wou: WearOutUpgrade = new WearOutUpgrade();
		wou.id = id;
		wou.displayName = displayName;
		wou.delta = delta;
		wou.durability = durability;
		wou.durabilityMax = durabilityMax;
		return wou;
	}
}
}