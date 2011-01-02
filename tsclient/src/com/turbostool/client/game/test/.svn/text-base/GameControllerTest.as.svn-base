package com.turbostool.client.game.test {

import com.turbostool.client.game.CarControlEventGenerator;
import com.turbostool.client.game.components.car.CarControlEvent;
import com.turbostool.client.game.components.car.RudderControl;

import flexunit.framework.TestCase;

public class GameControllerTest extends TestCase {
    public function GameControllerTest(methodName:String = null) {
        super(methodName);
    }

    public function testGameController():void {
        var cont:CarControlEventGenerator = new CarControlEventGenerator();

        function testControl(event:CarControlEvent):void {
            assertFalse(event.myAccelerate);
            assertEquals(RudderControl.NONE, event.myRudderControl);
        }

        var temp:Function = addAsync(testControl, 10);
        cont.addEventListener(CarControlEvent.CAR_CONTROL, //testRaceFinish);
                temp); //проверка на возникновение события
        var event: CarControlEvent = cont.checkEvents();
        cont.dispatchEvent(event);
        //! странно, но не работает! cont.removeEventListener(CarControlEvent.CAR_CONTROL, temp);
        /*var cont2:GameController = new GameController();
         function testControl2(event:CarControlEvent):void {
         assertTrue(event.myAccelerate);
         assertEquals(RudderControl.RIGHT, event.myRudderControl);
         }
         cont2.addEventListener(CarControlEvent.CAR_CONTROL, addAsync(testControl2, 10));
         cont2.addKeyCode(38);
         cont2.addKeyCode(39);
         cont2.checkEvents();*/
    }

}
}