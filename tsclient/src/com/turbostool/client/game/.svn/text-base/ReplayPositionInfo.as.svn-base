package com.turbostool.client.game{
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.Vector2d;

public class ReplayPositionInfo {

    private static const DELIMETER: String = ',';

    public var pos: Vector2d;
    public var rotation: Number;

    public function ReplayPositionInfo(pos: Vector2d, rotation: Number)
    {
        this.pos = pos;
        this.rotation = rotation;
    }

    public function equals(obj: Object): Boolean {
        if (obj == null) return false;
        if (this == obj) return true;
        if (!(obj is ReplayPositionInfo)) return false;
        var other: ReplayPositionInfo = obj as ReplayPositionInfo;
        return this.pos.equals(other.pos) && Utils.equal(this.rotation, other.rotation);
    }

    public static function equals2(o1: ReplayPositionInfo, o2: ReplayPositionInfo): Boolean {
        if (o1 == null && o2 == null) return true;
        if (o1 != null)
            return o1.equals(o2);
        else
            return o2.equals(o1);
    }

    public function encode() : String {
        return Utils.round(pos.myX, 4)
                + DELIMETER + Utils.round(pos.myY, 4)
                + DELIMETER + Utils.round(rotation, 4);
    }

    public static function decode(s: String): ReplayPositionInfo {
        var tokens: Array = s.split(DELIMETER);
        return new ReplayPositionInfo(
                new Vector2d(parseFloat(tokens[0]), parseFloat(tokens[1])),
                parseFloat(tokens[2])
                );
    }
}
}