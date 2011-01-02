package com.turbostool.client.game.components.car.test {
import flexunit.framework.TestSuite;

public class CarTestSuite extends TestSuite {
    public function CarTestSuite() {
        super(null);
        addTestSuite(CarEngineTest);
        addTestSuite(CarModelTest);
        addTest(CarTest.suite());
        addTestSuite(ChassisTest);
        addTest(HullTest.suite());
        addTest(RudderControlTest.suite());
        addTest(SuspensionTest.suite());
        addTest(CarTransmissionTest.suite());
        addTest(DifferentialTest.suite());
        addTest(CarBrakesTest.suite());
        addTestSuite(EditableTest);
        addTestSuite(WheelTest);
        addTestSuite(EngineParametersTest);
        addTestSuite(CarControlEventTest);
    }
}
}