package com.turbostool.client.game.route
{
import flash.events.Event;

public class TextureLoadResult extends Event
{
    public static const TEXTURE_LOADED: String = "textureLoaded";
    public static const LOAD_ERROR: String = "loadError";

    public var loader: SingleLoader;
    public var loaded: Boolean;
    public var bytesCount: int;

    public function TextureLoadResult(loaded: Boolean, loader: SingleLoader, bytesCount: int = -1)
    {
        super((loaded) ? TEXTURE_LOADED : LOAD_ERROR);
        this.loader = loader;
        this.loaded = loaded;
        this.bytesCount = bytesCount;
    }

}
}