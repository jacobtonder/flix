package ca.uwaterloo.flix;

import java.util.*;
import java.util.concurrent.locks.*;

public class SelectChannel {
    private List<SelectRule> rules;
    private List<Channel> channels;
    private Lock lock;
    private Condition condition;

    public SelectChannel(List<SelectRule> rules) {
        this.rules = rules;
        this.channels = new ArrayList<>();
        this.lock = new ReentrantLock();
        this.condition = this.lock.newCondition();

        for (SelectRule rule : rules) {
            this.channels.add((Channel) rule.getChannel());
        }

        Collections.sort(this.channels);
    }

    public void select() throws InterruptedException {
        Object result = null;

        lock();
        lockAllChannels();
        try {
            while (result == null) {
                result = pollChannels();

                if (result == null) {
                    addToAllChannels();
                    unlockAllChannels();
                    await();
                    lockAllChannels();
                }
            }
        }
        finally {
            unlockAllChannels();
            unlock();
        }
    }

    public void lock() {
        lock.lock();
    }

    public void unlock() {
        lock.unlock();
    }

    public void await() throws InterruptedException {
        condition.await();
    }

    public void signal() {
        condition.signalAll();
    }

    private void lockAllChannels() {
        for (Channel chan : channels) {
            chan.lock();
        }
    }

    private void unlockAllChannels() {
        for (Channel chan : channels) {
            chan.unlock();
        }
    }

    private void addToAllChannels() {
        for (Channel chan : channels) {
            chan.addSelect(this);
        }
    }

    private SelectRule pollChannels() {
        SelectRule result = null;
        for (SelectRule rule : rules) {
            if (rule.available()) {
                result = rule;
            }
        }

        return result;
    }
}
