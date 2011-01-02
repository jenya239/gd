package com.turbostool.client.net.messages {
public class SkipTutorialRequest extends ServerRequest{
    public static const SKIP_TUTORIAL: String = "skipTutorial";

    public function SkipTutorialRequest()
    {
        super(SKIP_TUTORIAL);
    }
}
}
