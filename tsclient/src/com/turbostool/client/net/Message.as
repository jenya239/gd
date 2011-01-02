package
{
public class Message
{
    private var myText:String;

    public function Message(text:String) {
        myText = (text == null) ? '' : text;
    }

    public function getText(): String {
        return myText;
    }

}
}