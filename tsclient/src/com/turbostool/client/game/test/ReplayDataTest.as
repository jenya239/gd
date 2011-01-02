package com.turbostool.client.game.test       {

import com.turbostool.client.game.ReplayData;
import com.turbostool.client.game.ReplayDataRecord;
import com.turbostool.client.game.ReplayPositionInfo;
import com.turbostool.client.game.components.car.CarControlEvent;
import com.turbostool.client.game.components.car.RudderControl;
import com.turbostool.client.utils.Vector2d;

import flexunit.framework.TestCase;

public class ReplayDataTest extends TestCase {

    public function ReplayDataTest(methodName:String = null) {
        super(methodName);
    }

    public function testAdd(): void {
        var replay: ReplayData = new ReplayData();

        replay.add(1, null, null);
        assertEquals(1, replay.Count);

        var event2: CarControlEvent = new CarControlEvent(false, RudderControl.NONE, false, false, 0, CarControlEvent.GEAR_NONE);
        var event4: CarControlEvent = new CarControlEvent(false, RudderControl.NONE, false, false, 1, CarControlEvent.GEAR_UP);
        var posInfo4: ReplayPositionInfo = new ReplayPositionInfo(new Vector2d(0, 0), 0);

        replay.add(2, event2, null);

        // same, but no duplicates
        replay.add(3, event2, null);

        // different
        replay.add(4, event4, posInfo4);

        assertEquals(4, replay.Count);

        replay.getNext();
        var rec2: ReplayDataRecord = replay.getNext();
        var rec3: ReplayDataRecord = replay.getNext();
        var rec4: ReplayDataRecord = replay.getNext();

        assertEquals(event2, rec2.event);
        assertEquals(null, rec3.event);
        assertEquals(event4, rec4.event);
        assertEquals(posInfo4, rec4.posInfo);
    }

    public function testHasNext(): void {
        var replay: ReplayData = new ReplayData();
        assertEquals(false, replay.hasNext());

        replay.add(1, null, null);
        assertEquals(true, replay.hasNext());
    }

    public function testGetNext(): void {

        var replay: ReplayData = new ReplayData();
        var rec: ReplayDataRecord;

        replay.add(1, null, null);
        rec = replay.getNext();
        assertNotNull(rec);
        assertEquals(1, rec.dt);
        assertEquals(null, rec.event);
        assertEquals(false, replay.hasNext());

        var event: CarControlEvent = new CarControlEvent(false, RudderControl.NONE, false, false, 0, CarControlEvent.GEAR_NONE);
        replay.add(1, event, null);
        assertEquals(true, replay.hasNext());
        rec = replay.getNext();
        assertNotNull(rec);
        assertEquals(1, rec.dt);
        assertEquals(event, rec.event);
        assertEquals(false, replay.hasNext());

        var event2: CarControlEvent = new CarControlEvent(false, RudderControl.NONE, false, false, 1, CarControlEvent.GEAR_NONE);
        var posInfo2: ReplayPositionInfo = new ReplayPositionInfo(new Vector2d(0, 0), 0);
        replay.add(1, event2, posInfo2);
        assertEquals(true, replay.hasNext());
        rec = replay.getNext();
        assertNotNull(rec);
        assertEquals(1, rec.dt);
        assertEquals(event2, rec.event);
        assertEquals(posInfo2, rec.posInfo);
        assertEquals(false, replay.hasNext());
    }

    public function testClear(): void {
        var replay: ReplayData = new ReplayData();
        replay.add(1, null, null);
        replay.clear();
        assertEquals(0, replay.Count);
    }

    public function testDecode(): void {
        var replay: ReplayData = new ReplayData();
        var event1: CarControlEvent = new CarControlEvent(true, RudderControl.LEFT, false, true, 0.1, CarControlEvent.GEAR_NONE);
        var event2: CarControlEvent = new CarControlEvent(true, RudderControl.LEFT, false, true, 0.2, CarControlEvent.GEAR_NONE);
        var posInfo2: ReplayPositionInfo = new ReplayPositionInfo(new Vector2d(0, 0), 0);
        var posInfo3: ReplayPositionInfo = new ReplayPositionInfo(new Vector2d(0, 0), 1);

        replay.add(0.55, null, null);
        replay.add(0.66, event1, null);
        replay.add(0.77, event2, posInfo2);
        replay.add(0.88, null, posInfo3);
        replay.add(0.99, null, null);

        var str: String = replay.encode();
        replay.clear();
        replay.decode(str);

        assertEquals(5, replay.Count);
        assertTrue((new ReplayDataRecord(0.55, null, null)).equals(replay.getNext()));
        assertTrue((new ReplayDataRecord(0.66, event1, null)).equals(replay.getNext()));
        assertTrue((new ReplayDataRecord(0.77, event2, posInfo2)).equals(replay.getNext()));
        assertTrue((new ReplayDataRecord(0.88, null, posInfo3)).equals(replay.getNext()));
        assertTrue((new ReplayDataRecord(0.99, null, null)).equals(replay.getNext()));

    }

    public function testEncode(): void {
        var replay: ReplayData = new ReplayData();
        var event: CarControlEvent = new CarControlEvent(true, RudderControl.LEFT, false, true, 0.1, CarControlEvent.GEAR_NONE);
        var event2: CarControlEvent = new CarControlEvent(true, RudderControl.LEFT, false, true, 0.2, CarControlEvent.GEAR_NONE);
        var posInfo2: ReplayPositionInfo = new ReplayPositionInfo(new Vector2d(0, 0), 0);
        replay.add(0.555, null, null);
        replay.add(0.666, event, null);
        replay.add(0.777, event2, posInfo2);
        replay.add(0.888, null, null);

        assertEquals("Replay: 0.555,0.666,[e:" + event.encode() + "],0.777,[e:" + event2.encode() + "],[p:" + posInfo2.encode() + "],0.888", replay.encode());
    }
}
}