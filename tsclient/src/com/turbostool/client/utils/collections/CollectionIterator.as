package com.turbostool.client.utils.collections {

public class CollectionIterator implements Iterator {
    public function CollectionIterator(collection:Collection)
    {
        this.collection = collection;
        cursor = 0;
    }

    //------------------------------------------------------------------------------

    public function hasNext():Boolean
    {
        return( cursor < collection.length() );
    }

    //------------------------------------------------------------------------------

    public function next():Object
    {
        if (cursor >= collection.length())
            throw new Error("Past end of collection");

        return( collection.getItemAt(cursor++) );
    }

    //------------------------------------------------------------------------------

    private var collection : Collection;
    public var cursor : uint;

}

}