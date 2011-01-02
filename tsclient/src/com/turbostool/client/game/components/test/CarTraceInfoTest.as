package com.turbostool.client.game.components.test
{
import com.turbostool.client.game.components.CarTraceInfo;
import com.turbostool.client.game.components.car.Car;
import com.turbostool.client.game.components.car.Wheel;
import com.turbostool.client.geom.Piece2d;
import com.turbostool.client.utils.Vector2d;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Iterator;

public class CarTraceInfoTest extends TestCase
{
    public function CarTraceInfoTest(methodName:String) {
        super(methodName);
    }

    public static function suite():TestSuite {
        var ts:TestSuite = new TestSuite();
        ts.addTest(new CarTraceInfoTest("testUpdateTraceQueueWithGrip"));
        ts.addTest(new CarTraceInfoTest("testUpdateTraceQueueWithoutGrip"));
        ts.addTest(new CarTraceInfoTest("testTraceQueueOverflow"));
        return ts;
    }

    public function testUpdateTraceQueueWithGrip():void {
        //create Car
        //road grip -> iterator is empty
        var car:Car = new Car();
        var carInfo:CarTraceInfo = new CarTraceInfo(car, 20);
        var it:Iterator = car.getCarModel().getChassis().getWheels().iterator();
        while (it.hasNext()) {
            var wheel:Wheel = (it.next() as Wheel);
            wheel.myStaticFriction = 1;
        }
        carInfo.updateTraceQueue();
        carInfo.updateTraceQueue();
        carInfo.updateTraceQueue();
        carInfo.updateTraceQueue();
        //sliding  = 0 => myRoadGrip = false
        it = carInfo.traceIterator();
        assertTrue(it.hasNext());
        while (it.hasNext()) {
            assertEquals(null, it.next());
        }
    }

    public function testUpdateTraceQueueWithoutGrip():void {
        var car:Car = new Car();
        var carInfo:CarTraceInfo = new CarTraceInfo(car, 20);
        var it:Iterator = car.getCarModel().getChassis().getWheels().iterator();
        var pieces:ArrayList = new ArrayList();
        while (it.hasNext()) {
            var wheel:Wheel = (it.next() as Wheel);
            var currR:Vector2d = new Vector2d(wheel.myR);
            var nextR:Vector2d = currR.sum2d(new Vector2d(1, 1));
            pieces.addItem(new Piece2d(currR, nextR));
            wheel.myStaticFriction = 0;
        }
        it = car.getCarModel().getChassis().getWheels().iterator();
        while (it.hasNext()) {
            var wee:Wheel = (it.next() as Wheel);
            var curryR:Vector2d = new Vector2d(wee.myR);
            wee.setR(curryR.sum2d(new Vector2d(1, 1)));
        }
        carInfo.updateTraceQueue();

        it = carInfo.traceIterator();
        var i:int = 0;
        while (it.hasNext()) {
            var pieceFromQueue:Piece2d = it.next() as Piece2d;
            var pieceEtalon:Piece2d = pieces.getItemAt(i++) as Piece2d;
            //trace(pieceFromQueue);
            assertTrue(pieceEtalon.equals(pieceFromQueue));
        }

        /*
         (1, -2, 0)
         (1, 2, 0)
         (-1, 2, 0)
         (-1, -2, 0)
         * */
    }

    public function testTraceQueueOverflow():void {
        var car:Car = new Car();
        var carInfo:CarTraceInfo = new CarTraceInfo(car, 3);
        var it:Iterator = car.getCarModel().getChassis().getWheels().iterator();
        while (it.hasNext()) {
            var wheel:Wheel = (it.next() as Wheel);
            wheel.myStaticFriction = 0;
        }

        for (var i:int = 0; i < 4; i++) {
            it = car.getCarModel().getChassis().getWheels().iterator();
            while (it.hasNext()) {
                var wool:Wheel = (it.next() as Wheel);
                var currR:Vector2d = new Vector2d(wool.myR);
                wool.setR(currR.sum2d(new Vector2d(1, 1)));
            }
            carInfo.updateTraceQueue();
        }

        it = carInfo.traceIterator();
        var n:int = 0;
        while (it.hasNext()) {
            n++;
            it.next();
        }
        assertEquals(12, n);
    }

}
}