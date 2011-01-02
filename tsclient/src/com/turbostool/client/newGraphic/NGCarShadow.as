package com.turbostool.client.newGraphic {
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.utils.Vector2d;

import flash.display.BitmapData;

public class NGCarShadow extends NGShadow {
    private static const LIGHT_ANGLE_X: Number = Math.PI / 12;
    private static const LIGHT_ANGLE_Y: Number = Math.PI / 16;

    private var myTexture: BitmapData;
    private var myCarView: NGCarView;


    public function NGCarShadow(carView: NGCarView, texture: BitmapData, transparent:Boolean = false) {
        super(carView.myFrame,
                NGShadow.internalCreatePolyFromRect(carView.myWidth, carView.myLength, carView.myHeight),
                texture, carView.myWidth, carView.myLength, transparent);
        myTexture = texture;
        myCarView = carView;
    }

    override protected function drawContent(ngg: NGGraphics): void {
        /*ngg.drawPolygon(myTexture, myCarView.myWidth, myCarView.myLength,
         NGShadow.internalCreatePolyFromRect(myCarView.myWidth, myCarView.myLength, 0)
         );*/
        myCarView.dirtyHack(ngg, myTexture);
    }

    override public function get myFrame():LocalFrame2d {
        var borderX: Number = myCarView.myHeight * Math.tan(LIGHT_ANGLE_X);
        var borderY: Number = myCarView.myHeight * Math.tan(LIGHT_ANGLE_Y);
        var result: LocalFrame2d = new LocalFrame2d(
                myCarView.myFrame.myR.difference2d(new Vector2d(borderX, borderY)),
                myCarView.myFrame.myAngle
                );
        return result;
    }

}
}