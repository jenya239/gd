package com.turbostool.client.ratings
{
import com.turbostool.client.ModelsStorage;
import com.turbostool.client.event.EventManager;
import com.turbostool.client.net.SessionSocket;
import com.turbostool.client.net.messages.GetPropertyRequest;
import com.turbostool.client.net.messages.GetPropertyResponse;
import com.turbostool.client.net.messages.ServerResponseEvent;

public class RatingsController
{
    private var _socket: SessionSocket;

    [Bindable]
    public var ratings: Array;

    [Bindable]
    public var modelsStorage: ModelsStorage;

    public function RatingsController(socket: SessionSocket, modelsStorage: ModelsStorage)
    {
        _socket = socket;
        this.modelsStorage = modelsStorage;

        EventManager.globalChannel.addEventListener(RefreshRatingsCommand.REFRESH_RATINGS_LIST, onRefreshRatings);
        EventManager.globalChannel.addEventListener(GetPropertyResponse.GET_RATINGS_RESPONSE, onRatingsResponse);
    }

    private function onRatingsResponse(event: ServerResponseEvent): void
    {
        var e: GetPropertyResponse = GetPropertyResponse(ServerResponseEvent(event).response);
        ratings = e.propertyAsArray;
    }

    private function onRefreshRatings(event: RefreshRatingsCommand): void
    {
        _socket.sendMessage(new GetPropertyRequest(GetPropertyRequest.RATINGS));
    }
}
}