package com.turbostool.client.game {
import com.turbostool.client.utils.collections.Collection;

public interface IGameComponentModel {
    function getDynamics():Collection;

    function getCollidables():Collection;
}
}