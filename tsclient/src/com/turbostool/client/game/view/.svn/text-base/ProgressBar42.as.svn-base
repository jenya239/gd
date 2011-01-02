package com.turbostool.client.game.view {
import com.turbostool.client.utils.Assert;

import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.display.Graphics;
import flash.display.Sprite;
import flash.geom.ColorTransform;
import flash.geom.Matrix;

import mx.preloaders.DownloadProgressBar;

public class ProgressBar42 {

    private static const COUNT_X: int = 3;
    private static const COUNT_Y: int = 3;

    [Embed(source="/assets/gui/preloader/car_small.png")]
    [Bindable]
    private var carImgClass: Class;
    private var carImgBitmap: BitmapData;

    [Embed(source="/assets/gui/preloader/colormap.png")]
    [Bindable]
    private var colormapClass: Class;
    private var colormapBitmap: BitmapData;

    [Embed(source="/assets/gui/preloader/logo.png")]
    [Bindable]
    private var gdLogoClass: Class;
    private var gdLogoBitmap: BitmapData;

    private var myCanvas: Sprite;
    private var myTileSizeX: Number;
    private var myTileSizeY: Number;
    private var myProgressBegin: Number;
    private var myProgressEnd: Number;

    public function ProgressBar42(canvas: Sprite, progressBegin: Number = 0, progressEnd: Number = 1) {
        carImgBitmap = (new carImgClass() as Bitmap).bitmapData;
        colormapBitmap = (new colormapClass() as Bitmap).bitmapData;
        gdLogoBitmap = (new gdLogoClass() as Bitmap).bitmapData;
        myCanvas = canvas;
        myTileSizeX = carImgBitmap.width;
        myTileSizeY = carImgBitmap.height;
        myProgressBegin = Math.max(progressBegin, 0);
        myProgressEnd = Math.min(progressEnd, 1);
        Assert.assertTrue(myProgressBegin <= myProgressEnd);
    }

    public function drawProgress(progress: Number): void {
        var trueProgress: Number = myProgressBegin + progress * (myProgressEnd - myProgressBegin);
        var state: Number = Math.round(COUNT_X * COUNT_Y * trueProgress);
        var carCount: Number = 0;
        var graphics: Graphics = myCanvas.graphics;

        var canvasWidth: Number = myCanvas.width;
        var canvasHeight: Number = myCanvas.height;
        if (myCanvas is DownloadProgressBar) {
            canvasWidth = (myCanvas as DownloadProgressBar).stageWidth
            canvasHeight = (myCanvas as DownloadProgressBar).stageHeight
        }

        var deltaX: Number = -(COUNT_X * myTileSizeX - canvasWidth) / 2;
        var deltaY: Number = -(COUNT_Y * myTileSizeY + gdLogoBitmap.height - canvasHeight) / 2;

        graphics.clear();
        graphics.beginFill(0);
        graphics.drawRect(0, 0, canvasWidth, canvasHeight);
        graphics.endFill();

        var gdX: Number = COUNT_X * myTileSizeX + deltaX - gdLogoBitmap.width;
        var gdY: Number = COUNT_Y * myTileSizeY + deltaY;
        var m: Matrix = new Matrix();
        m.translate(gdX, gdY);
        graphics.beginBitmapFill(gdLogoBitmap, m);
        graphics.drawRect(gdX, gdY, gdLogoBitmap.width, gdLogoBitmap.height);
        graphics.endFill();

        m = new Matrix();
        m.scale(myTileSizeX / carImgBitmap.width, myTileSizeY / carImgBitmap.height);
        m.translate(deltaX, deltaY);

        for (var row: int = 1; row <= COUNT_Y; row++) {
            for (var col: int = 1; col <= COUNT_X; col++) {
                var xInd: Number;
                var yInd: Number = row - 1;
                if (row % 2 == 1) {
                    xInd = col - 1;
                } else {
                    xInd = COUNT_X - col;
                }
                var color: uint = colormapBitmap.getPixel(xInd, yInd);
                var r: Number = (color & 0xff0000) / 0xff0000;
                var g: Number = (color & 0xff00  ) / 0xff00;
                var b: Number = (color & 0xff    ) / 0xff;
                var bitmap: BitmapData = carImgBitmap.clone();
                var trans: ColorTransform = new ColorTransform(r, g, b);
                bitmap.colorTransform(bitmap.rect, trans);

                var xCoord: Number = xInd * myTileSizeX + deltaX;
                var yCoord: Number = yInd * myTileSizeY + deltaY;

                graphics.beginBitmapFill(bitmap, m);
                graphics.drawRect(xCoord, yCoord, myTileSizeX, myTileSizeY);
                graphics.endFill();

                carCount++
                if (carCount == state) {
                    return;
                }
            }
            m.translate(-deltaX, -deltaY);
            m.rotate(Math.PI);
            m.translate(deltaX, deltaY);
        }
    }

}
}