package com.turbostool.client.game {
import com.turbostool.client.newGraphic.NGDrawable;
import com.turbostool.client.utils.collections.*;

public interface IGameComponent {
    function getClassName():String;

    function getName():String;

    function getNGDrawable(): NGDrawable;

    function getModels():Collection;
}
}