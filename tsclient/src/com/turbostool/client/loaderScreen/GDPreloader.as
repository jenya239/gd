package  com.turbostool.client.loaderScreen
{
import flash.display.*;
import flash.events.*;

import flash.filters.BitmapFilterQuality;
import flash.filters.BlurFilter;
import flash.filters.GlowFilter;
import flash.text.TextField;

import flash.text.TextFieldAutoSize;
import flash.text.TextFormat;

import flash.text.engine.FontWeight;

import mx.events.*;
import mx.preloaders.*;

public class GDPreloader extends DownloadProgressBar {

    [ Embed(source="/assets/gui/preloader/preloader.swf") ]
    public var RollingThingClass:Class;

    public function GDPreloader()
    {
        super();
        var clip: MovieClip = new RollingThingClass();
        addChild(clip);

				/*clip.alpha = 0.9;
				graphics.beginFill(0xffffff, 0.9);
				graphics.drawRect(0, 0, 1000, 1000);
				graphics.endFill();
				var txt: TextField = new TextField();
				txt.x = 50;
				txt.y = 180;
				txt.autoSize = TextFieldAutoSize.LEFT;
				txt.defaultTextFormat = new TextFormat( 'Arial', 22, 0, FontWeight.BOLD );
				txt.filters = [new GlowFilter(0xffffff, 0.5, 9, 9)];
				txt.text = "Каникулы в Городе Дорог. \nПо крайней мере до 11.11.11 11:11:11\n\nUPD: Каникулы продлены.";
				clip.filters = [ new BlurFilter( 9, 9, BitmapFilterQuality.HIGH ) ];
				addChild(txt);*/
    }

    override public function set preloader(preloader:Sprite):void
    {
        preloader.addEventListener(ProgressEvent.PROGRESS, onLoadProgress);
        preloader.addEventListener(FlexEvent.INIT_PROGRESS, onInitProgress);
        preloader.addEventListener(FlexEvent.INIT_COMPLETE, onInitEnd);
    }

    private function onInitProgress(e:Event):void
    {
    }

    private function onLoadProgress(event: ProgressEvent):void
    {
        //trace("Preloaded : " + event.bytesLoaded );
    }

    private function onInitEnd(event:Event):void
    {
        dispatchEvent(new Event(Event.COMPLETE));
    }

}
}