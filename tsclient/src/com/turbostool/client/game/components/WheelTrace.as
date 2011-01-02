package com.turbostool.client.game.components
{
import com.turbostool.client.game.IGameComponent;
import com.turbostool.client.game.RaceModel;
import com.turbostool.client.newGraphic.NGDrawable;
import com.turbostool.client.utils.TSError;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;

public class WheelTrace implements IGameComponent {
    private var myWheelTraceModel:WheelTraceModel;

    public function WheelTrace(raceModel:RaceModel) {
        myWheelTraceModel = new WheelTraceModel(raceModel);
    }

    public function getClassName():String
    {
        return 'WheelTrace';
    }

    public function getName():String
    {
        return 'Wheel_Trace';
    }

    public function getModels():Collection
    {
        return new ArrayList(myWheelTraceModel);
    }

    public function getDrawables():Collection
    {
        return new ArrayList();
    }

    public function getModel():WheelTraceModel {
        return myWheelTraceModel;
    }

    public function getNGDrawable(): NGDrawable {
        throw new TSError("doesn't support NGDrawable ");
        return null;
    }
}
}