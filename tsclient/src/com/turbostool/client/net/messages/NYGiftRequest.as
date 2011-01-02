package com.turbostool.client.net.messages {
public class NYGiftRequest extends ServerRequest{
    public static const NY_GIFT: String = "nyGift";

    public function NYGiftRequest()
    {
        super(NY_GIFT);
    }
}
}
