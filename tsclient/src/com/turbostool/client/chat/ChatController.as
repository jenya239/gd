package com.turbostool.client.chat
{
import com.turbostool.client.event.EventManager;
import com.turbostool.client.model.ChatMessageInfo;
import com.turbostool.client.net.messages.ChatMessageMessage;

import com.turbostool.client.net.messages.ServerResponseEvent;

import mx.collections.ArrayCollection;

public class ChatController
{
    [Bindable]
    public var chatMessagesCity: ArrayCollection = new ArrayCollection();

    [Bindable]
    public var chatMessagesTrade: ArrayCollection = new ArrayCollection();

    [Bindable]
    public var chatMessagesLobby: ArrayCollection = new ArrayCollection();

    [Bindable]
    public var chatMessagesPrivate: ArrayCollection = new ArrayCollection();

    public function ChatController()
    {
        EventManager.cityChannel.addEventListener(ChatMessageMessage.CHAT_MESSAGE, onChatMessageCity);
        EventManager.tradeChannel.addEventListener(ChatMessageMessage.CHAT_MESSAGE, onChatMessageTrade);
        EventManager.lobbyRaceChannel.addEventListener(ChatMessageMessage.CHAT_MESSAGE, onChatMessageLobby);
        EventManager.privateChannel.addEventListener(ChatMessageMessage.CHAT_MESSAGE, onChatMessagePrivate);
    }

    private function onChatMessageCity(event: ServerResponseEvent): void
    {
        var chatMessage: ChatMessageMessage = event.response as ChatMessageMessage;
        chatMessagesCity.addItem(new ChatMessageInfo(chatMessage.timeStampDate, chatMessage.nick, chatMessage.text, chatMessage.userId, chatMessage.homeCity,chatMessage.admin,chatMessage.rj));
    }

    private function onChatMessageTrade(event: ServerResponseEvent): void
    {
        var chatMessage: ChatMessageMessage = event.response as ChatMessageMessage; 
        chatMessagesTrade.addItem(new ChatMessageInfo(chatMessage.timeStampDate, chatMessage.nick, chatMessage.text, chatMessage.userId, chatMessage.homeCity,chatMessage.admin,chatMessage.rj));
    }

    private function onChatMessageLobby(event: ServerResponseEvent): void
    {
        var chatMessage: ChatMessageMessage = event.response as ChatMessageMessage;
        chatMessagesLobby.addItem(new ChatMessageInfo(chatMessage.timeStampDate, chatMessage.nick, chatMessage.text, chatMessage.userId, chatMessage.homeCity,chatMessage.admin,chatMessage.rj));
    }

    private function onChatMessagePrivate(event: ServerResponseEvent): void
    {
        var chatMessage: ChatMessageMessage = event.response as ChatMessageMessage;
        chatMessagesPrivate.addItem(new ChatMessageInfo(chatMessage.timeStampDate, chatMessage.nick, chatMessage.text, chatMessage.userId, chatMessage.homeCity,chatMessage.admin,chatMessage.rj));
    }
}
}