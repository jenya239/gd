package com.turbostool.client.game.test {
import com.turbostool.client.game.*;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.utils.*;

public class RaceModelTest extends TestCase {
    private var myModel:RaceModel;

    override public function setUp():void {
        myModel = new RaceModel();
        myModel.setRaceWorld(new RaceWorld(null, new Piece2d(new Vector2d(-1, 0), new Vector2d(1, 0)), null, null, null));
    }

    public function RaceModelTest(methodName:String = null) {
        super(methodName);
    }

    private function modelStep():void {
        myModel.checkEvents();
    }

    /*
     public function testLap():void {
     var lapCount:int = 0;
     var car:Car = new Car();
     car.myId = Car.LOCAL_CAR_ID;
     myModel.getCars().addItem(car);
     var startHandled: Boolean = false;

     function timeHandler(e: LapTimeEvent, startOk:Boolean, finishOk: Boolean):void
     {
     if (e.timeType == LapTimeEvent.START)
     {
     if (startOk)
     {
     startHandled = true;
     assertTrue(e.time > 0);
     } else
     {
     fail();
     }
     }
     else
     if (e.timeType == LapTimeEvent.FINISH)
     {
     if (finishOk)
     {
     finishHandled = true;
     } else
     {
     fail();
     }
     }
     }

     function timeHandler_StartOk_FinishFail(e: LapTimeEvent):void
     {
     timeHandler(e, true, false);
     }

     function timeHandler_StartOk_FinishOk(e: LapTimeEvent):void
     {
     timeHandler(e, true, true);
     }

     function timeHandler_StartFail_FinishFail(e: LapTimeEvent):void
     {
     timeHandler(e, false, false);
     }

     //тут будет комментарий

     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, -1);
     modelStep();
     myModel.addEventListener(LapTimeEvent.LAP_TIME_LOCAL, timeHandler_StartOk_FinishFail);
     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, 1);
     modelStep();
     myModel.removeEventListener(LapTimeEvent.LAP_TIME_LOCAL, timeHandler_StartOk_FinishFail);
     assertTrue(startHandled);

     //нормальный финиш
     startHandled = false;
     var finishHandled: Boolean = false;
     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, -1);
     modelStep();
     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, -1);
     modelStep();
     myModel.addEventListener(LapTimeEvent.LAP_TIME_LOCAL, timeHandler_StartOk_FinishOk);
     modelStep();
     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, 1);
     modelStep();
     car.getCarModel().myHull.myFrame.myR = new Vector2d(2, 1);
     modelStep();
     car.getCarModel().myHull.myFrame.myR = new Vector2d(2, -1);
     modelStep();
     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, -1);
     modelStep();
     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, 1);
     modelStep();
     myModel.removeEventListener(LapTimeEvent.LAP_TIME_LOCAL, timeHandler_StartOk_FinishOk);
     assertTrue(startHandled && finishHandled);

     //проезд назад
     //car.myLastPosition = new Vector2d(0, 1);
     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, -1);
     myModel.addEventListener(LapTimeEvent.LAP_TIME_LOCAL, timeHandler_StartFail_FinishFail);
     modelStep();
     myModel.removeEventListener(LapTimeEvent.LAP_TIME_LOCAL, timeHandler_StartFail_FinishFail);


     //опять вперед
     startHandled = false;
     myModel.addEventListener(LapTimeEvent.LAP_TIME_LOCAL, timeHandler_StartOk_FinishFail);
     //car.myLastPosition = new Vector2d(0, -1);
     car.getCarModel().myHull.myFrame.myR = new Vector2d(0, 1);
     modelStep();
     myModel.removeEventListener(LapTimeEvent.LAP_TIME_LOCAL, timeHandler_StartOk_FinishFail);
     assertTrue(startHandled);

     }
     */
}
}