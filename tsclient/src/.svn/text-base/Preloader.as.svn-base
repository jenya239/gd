package {
import com.turbostool.client.game.view.ProgressBar42;

import flash.display.*;
import flash.events.*;
import flash.utils.*;

import mx.events.*;
import mx.preloaders.*;

public class Preloader extends DownloadProgressBar {

    public static const INIT_PROGRESS_PART: Number = 0.3;

    private var myProgressBar42: ProgressBar42;

    public function Preloader() {
        super();
        myProgressBar42 = new ProgressBar42(this, 0, INIT_PROGRESS_PART);
    }

    override public function set preloader(preloader:Sprite):void {
        preloader.addEventListener(ProgressEvent.PROGRESS, myHandleProgress);

        preloader.addEventListener(FlexEvent.INIT_COMPLETE, myHandleInitEnd);
    }

    private function myHandleProgress(event:ProgressEvent):void {
        myProgressBar42.drawProgress(event.bytesLoaded / event.bytesTotal);
    }

    private function myHandleInitEnd(event:Event):void {
        function dispatchComplete(event:TimerEvent):void {
            dispatchEvent(new Event(Event.COMPLETE));
        }

        var timer:Timer = new Timer(0000, 1);
        timer.addEventListener(TimerEvent.TIMER, dispatchComplete);
        timer.start();
    }

}
}