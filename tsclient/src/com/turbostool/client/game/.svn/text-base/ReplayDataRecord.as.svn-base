package com.turbostool.client.game {

import com.turbostool.client.game.components.car.CarControlEvent;

public class ReplayDataRecord {
    public var dt: Number;
    public var event: CarControlEvent;
    public var posInfo: ReplayPositionInfo;

    public function ReplayDataRecord(dt: Number, event: CarControlEvent, posInfo: ReplayPositionInfo) {
        this.dt = dt;
        this.event = event;
        this.posInfo = posInfo;
    }

    public function encode(): String {
        var str: String;
        str += this.dt;
        if (this.event != null) {
            str += ',[e:' + this.event.encode() + ']';
        }
        if (this.posInfo != null) {
            str += ',[p:' + this.posInfo.encode() + ']';
        }
        return str;
    }

    public function equals(obj: Object): Boolean {
        if (obj == null) return false;
        if (this == obj) return true;
        if (!(obj is ReplayDataRecord)) return false;
        var other: ReplayDataRecord = obj as ReplayDataRecord;
        return     this.dt == other.dt &&
                   CarControlEvent.equals2(this.event, other.event) &&
                   ReplayPositionInfo.equals2(this.posInfo, other.posInfo);
    }
}
}