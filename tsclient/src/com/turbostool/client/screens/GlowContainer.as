package com.turbostool.client.screens {
import mx.containers.Canvas;
import mx.effects.Glow;

public class GlowContainer extends Canvas{
    [Bindable]
	public var buttonGlowIn: Glow;

    [Bindable]
	public var buttonGlowOut: Glow;

    [Bindable]
	public var buttonGlowIn2: Glow;

    [Bindable]
    public var buttonHalfGlowIn: Glow;    

	public function GlowContainer() {
		super();
        buttonHalfGlowIn = new Glow();
		buttonGlowIn = new Glow();
		buttonGlowIn2 = new Glow();
		buttonGlowOut = new Glow();

		buttonGlowIn.color = 0xeeeeee;
		buttonGlowIn.alphaFrom = 0.1;
		buttonGlowIn.alphaTo = 0.5;
		buttonGlowIn.duration = 100;
		buttonGlowIn.blurYFrom = 0;
		buttonGlowIn.blurXFrom = 0;
		buttonGlowIn.blurYTo = 10;
		buttonGlowIn.blurXTo = 10;

		buttonGlowIn2.color = 0xeeeeee;
		buttonGlowIn2.alphaFrom = 0.1;
		buttonGlowIn2.alphaTo = 0.6;
		buttonGlowIn2.duration = 100;
		buttonGlowIn2.blurYFrom = 0;
		buttonGlowIn2.blurXFrom = 0;
		buttonGlowIn2.blurYTo = 20;
		buttonGlowIn2.blurXTo = 20;

		buttonGlowOut.color = 0xeeeeee;
		buttonGlowOut.alphaFrom = 0.5;
		buttonGlowOut.alphaTo = 0.1;
		buttonGlowOut.duration = 800;
		buttonGlowOut.blurYFrom = 10;
		buttonGlowOut.blurXFrom = 10;
		buttonGlowOut.blurYTo = 0;
		buttonGlowOut.blurXTo = 0;

        buttonHalfGlowIn.color = 0xeeeeee;
		buttonHalfGlowIn.alphaFrom = 0.1;
		buttonHalfGlowIn.alphaTo = 0.25;
		buttonHalfGlowIn.duration = 100;
		buttonHalfGlowIn.blurYFrom = 0;
		buttonHalfGlowIn.blurXFrom = 0;
		buttonHalfGlowIn.blurYTo = 10;
		buttonHalfGlowIn.blurXTo = 10;
	}
}
}