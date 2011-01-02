package com.turbostool.carEditor
{
import com.turbostool.client.utils.Utils;

import flash.display.DisplayObjectContainer;
import flash.events.Event;

import mx.binding.utils.BindingUtils;
import mx.binding.utils.ChangeWatcher;
import mx.controls.TextInput;

public class NumberInput extends TextInput
{
    private var _chain: Array = null;
    private var _watcher: ChangeWatcher;

    public var coef: Number = 1;
    public var precision: Number = 2;

    public function NumberInput()
    {
        addEventListener(Event.CHANGE, onChange);
    }

    private function onChange(e: Event): void
    {
        if (_chain == null) return;
        var obj: Object = parent;
        for (var i: int = 0; i < _chain.length - 1; i++)
        {
            obj = obj[_chain[i]];
        }
        obj[_chain[_chain.length - 1]] = parseFloat(text) / coef;
    }

    public function set bindProperty(props: String): void
    {
        _chain = props.split('.');
    }

    private function watch(): void
    {
        if (_watcher != null) _watcher.unwatch();
        _watcher = BindingUtils.bindSetter(function(val: Number):void {
            text = Utils.round(val * coef, precision).toString();
        }, parent, _chain);
    }

    override public function parentChanged(p:DisplayObjectContainer):void
    {
        super.parentChanged(p);
        watch();
    }
}
}