package  com.turbostool.client.loaderScreen
{
import flash.display.*;
import flash.events.*;

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