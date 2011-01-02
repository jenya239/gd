package com.turbostool.client.ratings
{
import flash.events.Event;

public class RefreshRatingsCommand extends Event
{
    public static const REFRESH_RATINGS_LIST: String = "refreshRatingsCommand";

    public function RefreshRatingsCommand()
    {
        super(REFRESH_RATINGS_LIST);
    }
}
}