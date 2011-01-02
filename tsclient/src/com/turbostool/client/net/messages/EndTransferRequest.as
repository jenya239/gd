package com.turbostool.client.net.messages
{
public class EndTransferRequest extends ServerRequest
{
    public static const END_TRANSFER: String = "endTransfer";

    public function EndTransferRequest() {
        super(END_TRANSFER);
    }
}
}