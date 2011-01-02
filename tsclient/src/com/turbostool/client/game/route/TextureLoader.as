package com.turbostool.client.game.route
{
import com.turbostool.client.Config;
import com.turbostool.client.event.ProgressEvent2;
import com.turbostool.client.utils.Utils;
import com.turbostool.client.utils.collections.ArrayList;
import com.turbostool.client.utils.collections.Collection;
import com.turbostool.client.utils.collections.HashMap;
import com.turbostool.client.utils.collections.Iterator;
import com.turbostool.client.utils.collections.Queue;

import flash.display.Bitmap;
import flash.display.BitmapData;
import flash.events.Event;
import flash.events.EventDispatcher;
import flash.events.ProgressEvent;

public class TextureLoader extends EventDispatcher
{
    private static const SLOT_COUNT: int = 1;
    public static const ALL_TEXTURES_LOADED: String = "WOW!! ALL textures are loaded!!";

    [Embed(source="/assets/textures/imageNotLoaded.png")]
    private static var _failedImgClass: Class;

    private static var _failedImgBitmap: BitmapData = (new _failedImgClass() as Bitmap).bitmapData;

    private var myTextures: HashMap = new HashMap();
    private var myLoadingQueue: Queue = new Queue();
    private var myBitmapLoaderList: ArrayList;

    private var myTexturesCount: Number;
    private var myTexturesLoaded: Number;
    private var myBytesLoaded: Number;
    private var myTimerStart: Number;
    public static var ourPrefix: String = 'data/routes/textures/';
    public var myPrefix: String = ourPrefix;

    private var myLoaders: ArrayList = new ArrayList();

    public function TextureLoader()
    {
        //_failedImgBitmap = (new failedImgClass() as Bitmap).bitmapData;
        myTexturesCount = 0;
        myBitmapLoaderList = new ArrayList();
        for (var i: int = 0; i < SLOT_COUNT; i++)
        {
            var loader: SingleLoader = new SingleLoader();
            loader.addEventListener(TextureLoadResult.TEXTURE_LOADED, bitmapLoadedHandler);
            loader.addEventListener(TextureLoadResult.LOAD_ERROR, bitmapLoadedHandler);
            myBitmapLoaderList.addItem(loader);
        }
    }

    private function removeDuplicates(coll: Collection): Collection {
        var result: ArrayList = new ArrayList();
        var it: Iterator = coll.iterator();
        while (it.hasNext()) {
            var o: Object = it.next();
            if (result.contains(o) || myLoadingQueue.contains(o) || myTextures.containsKey(o)) {
                continue;
            }
            result.addItem(o);
        }
        return result;
    }

    public function load(urlList: Collection): void {
        var loading: Boolean = !myLoadingQueue.isEmpty();
        var newTextures:Collection = removeDuplicates(urlList);
        myLoadingQueue.addCollection(newTextures);
        myTexturesCount += newTextures.length();
        //myTexturesCount = myLoadingQueue.length();
        if (!loading) {
            myTexturesLoaded = 0;
            myBytesLoaded = 0;
            myTimerStart = Utils.now();
            loadNext();
        }
        updateProgress();
    }

    private function updateProgress(): void
    {
        var progress: ProgressEvent = new ProgressEvent(ProgressEvent.PROGRESS);
        progress.bytesLoaded = myTexturesLoaded;
        progress.bytesTotal = myTexturesCount;
        dispatchEvent(progress);
        dispatchEvent(new ProgressEvent2(myTexturesLoaded, myTexturesCount, myBytesLoaded));
    }

    private function bitmapLoadedHandler(e: TextureLoadResult): void {
        myTexturesLoaded++;
        myBytesLoaded += e.bytesCount;

        updateProgress();

        var loadedTexture: BitmapData;
        if (e.loaded) {
            loadedTexture = (e.loader.content as Bitmap).bitmapData;
        } else {
            loadedTexture = _failedImgBitmap;
            trace("failed: " + e.loader.key);
        }
        myTextures.setValue(e.loader.key, loadedTexture); //((e.loaded) ? Bitmap(e.loader.content).bitmapData : failedImgBitmap)
        loadNext();
        //trace(e.loader.key);
        /*
         if (myTexturesLoaded < myTexturesCount)
         {
         loadNext();
         } else
         {
         var timerEnd: Number = Utils.now();
         var seconds: Number = (timerEnd-myTimerStart)/1000;
         var speed: Number = myBytesLoaded / 1024 / seconds;
         //trace((myBytesLoaded /1024)+ " " + seconds + " " + speed);
         dispatchEvent(new Event(ALL_TEXTURES_LOADED));
         }
         */
    }

    public static function getRandomSuffix(): String
    {
        //return "?" + Math.round(Math.random() * 10000000000);
	    return "?" + 1234567890;
    }

    private function findEmptySlot(): SingleLoader
    {
        for (var i: int = 0; i < myBitmapLoaderList.length(); i++)
        {
            var loader: SingleLoader = myBitmapLoaderList.getItemAt(i) as SingleLoader;
            if (!loader.isLoading)
            {
                return loader;
            }
        }
        return null;
    }

    private function getNumberOfFreeSlots(): int
    {
        var freeSlotsCount: int = 0;
        for (var i: int = 0; i < myBitmapLoaderList.length(); i++)
        {

            if ((myBitmapLoaderList.getItemAt(i) as SingleLoader).isLoading)
            {
                freeSlotsCount++;
            }
        }
        return freeSlotsCount;
    }

    private function loadNext(): void
    {
        if (myTexturesLoaded >= myTexturesCount
                || myLoadingQueue.isEmpty())
        //if (myLoadingQueue.isEmpty())
        {
            dispatchEvent(new ProgressEvent2(myTexturesCount, myTexturesCount, myBytesLoaded));
            dispatchEvent(new Event(ALL_TEXTURES_LOADED));
            return;
        }
        else
        {
            while (true)
            {
                var freeLoader:SingleLoader = findEmptySlot();
                if (freeLoader == null)
                {
                    break;
                }
                var url: String = myLoadingQueue.pop() as String;
                freeLoader.loadTexture((Config.initialized ? Config.instance.serverUrl : "") + myPrefix + url + getRandomSuffix(), url);
                //trace(getNumberOfFreeSlots());
            }
        }
    }

    public function getTexture(url: String): BitmapData {
        var result: BitmapData = myTextures.getValue(url) as BitmapData;
        return result != null ? result : _failedImgBitmap;
    }

    public function getTextureUrls(): Array {
        return myTextures.getKeys();
    }

    public static function get failedImgBitmap(): BitmapData
    {
        return _failedImgBitmap;
    }
}
}