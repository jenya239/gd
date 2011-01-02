package com.turbostool.client.game.components.car.test {

import com.turbostool.client.game.components.car.*;

import flexunit.framework.TestCase;

public class CarControlEventTest extends TestCase {
    private var myEvent: CarControlEvent;

    public function CarControlEventTest(methodName:String = null) {
        super(methodName);
    }

    override public function setUp(): void {
        myEvent = new CarControlEvent(true, 7, true, false, 2.3, CarControlEvent.GEAR_UP);
    }

    public function testEncoding():void {
        assertTrue(myEvent.equals(CarControlEvent.decode(myEvent.encode())));
    }

    public function testClone():void {
        assertTrue(myEvent.equals(myEvent.clone()));
    }


}
}