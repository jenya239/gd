package com.turbostool.client.utils.collections
{
import mx.collections.ArrayCollection;

public class FilteredCollection extends ArrayCollection
{
    public function FilteredCollection(array: Array, filter: Function)
    {
        super(array);
        filterFunction = filter;
        this.refresh();
    }
}
}