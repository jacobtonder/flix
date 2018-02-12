package ca.uwaterloo.flix;

public class SelectRule {
    private String ident;
    private Channel channel;
    private Object value;
    private Object result;

    public SelectRule(String ident, Channel channel) {
        this.ident = ident;
        this.channel = channel;
    }

    public String getIdent() {
        return ident;
    }

    public Object getResult() {
        return this.result;
    }

    public Object getChannel() {
        return this.channel;
    }

    public void evaluate() {
        this.result = (int)this.value + 2;
        // code ...
    }

    public boolean available() {
        boolean isAvailable = this.itemAvailable();

        if (isAvailable) {
            this.value = this.channel.poll();

            this.evaluate();

            this.channel.signalNotFull();
        }

        return isAvailable;
    }

    private boolean itemAvailable() {
        return this.channel.nonEmpty() && this.result.equals(null);
    }
}
