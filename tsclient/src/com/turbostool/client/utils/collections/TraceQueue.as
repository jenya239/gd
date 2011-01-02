package com.turbostool.client.utils.collections
{
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.newGraphic.LayerConstants;
import com.turbostool.client.newGraphic.NGGraphicEngine;
import com.turbostool.client.newGraphic.NGTexturedRect;
import com.turbostool.client.utils.Vector2d;

import flash.display.Bitmap;
import flash.display.BitmapData;

public class TraceQueue
{
    [Embed(source="/assets/textures/tire_track.png")]
    [Bindable]
    private var traceImgCls: Class;

    private var myTraceTexture: BitmapData;

    private var myArray:ArrayList;
    private var myCursor:int;
    private var myMaxSize:int;
    private var myEngine: NGGraphicEngine;

    public function TraceQueue(size: int) {
        myMaxSize = size;
        myArray = new ArrayList();
        myCursor = 0;
        myEngine = NGGraphicEngine.instance;
        myTraceTexture = (new traceImgCls() as Bitmap).bitmapData;
    }

    public function setItem(begin:Vector2d, end:Vector2d):void {
        if (myArray.toArray()[myCursor] != null) {
            myEngine.removeDrawable(myArray.toArray()[myCursor]);
        }
        myArray.toArray()[myCursor] = new NGTexturedRect(
                new LocalFrame2d(begin.sum2d(end).multiply2d(0.5), end.difference2d_shared(begin).getAngle()),
                0,
                LayerConstants.WHEEL_TRACE,
                end.difference2d_shared(begin).length(),
                0.18,
                myTraceTexture,
                true //было false и следы были с желтизной,
            //так как под прозрачной текстурой все желтое (см NGTexturedRect)
            //ToDo отпрофайлить, чтобы понять насколько тормозит прозрачность в следах
                );
        myEngine.addDrawable(myArray.toArray()[myCursor]);

        myCursor++;
        if (myCursor == myMaxSize) {
            myCursor = 0;
        }
    }

    public function iterator():Iterator {
        return myArray.iterator();
    }
}
}