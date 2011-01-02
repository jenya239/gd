package com.turbostool.client.dynamicEngine {
import com.turbostool.client.geom.LocalFrame2d;
import com.turbostool.client.utils.Vector2d;

public interface ICameraLinkable {
    function get myFrame():LocalFrame2d;

    function get myVelocity():Vector2d;
}
}