package com.turbostool.client.event
{
import com.turbostool.client.game.RaceWorld;

import flash.events.Event;

public class WorldCreatedEvent extends Event
{
    public static const WORLD_CREATED: String = "worldCreatedEvent";  
    private var _world: RaceWorld;

    public function WorldCreatedEvent(world: RaceWorld)
    {
        super(WORLD_CREATED);
        _world = world;
    }

    public function get world():RaceWorld
    {
        return _world;
    }
}
}