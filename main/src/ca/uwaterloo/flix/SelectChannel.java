package ca.uwaterloo.flix;

import javafx.util.Pair;
import java.util.*;
import java.util.concurrent.locks.*;

public class SelectChannel {
    public Pair<Integer, Object> run(List<Channel> channels) {
        Lock sLock = new ReentrantLock();
        Condition sCondition = sLock.newCondition();

        Pair<Integer, Object> = result = null;

        lockAllChannels(channels);
        sLock.lock();
        try {
            while (result == null) {
                for (Iterator<Channel> iterator = channels.iterator(); iterator.hasNext(); ) {
                    ChannelInt chan = (ChannelInt)iterator.next();

                    if (chan.isNonEmpty() && result == null) {
                        Object value = chan.poll();
                        result = new Pair(chan.id, value);

                        chan.signalGetters();
                    }
                }

                if (result == null) {
                    addConditions(channels, sLock, sCondition);
                    unlockAllChannels(channels);
                    sCondition.await();
                    lockAllChannels(channels);
                }
            }
        }
        finally {
            unlockAllChannels(channels);
            sLock.unlock();
        }

        return result;
    }

//  public Pair<Object, Closure> selectChannel(List<Pair<Channel, Closure>> rules) throws InterruptedException {
    public Pair<Object, Object> selectChannel(List<Pair<Channel, Object>> rules) throws InterruptedException {
        Lock sLock = new ReentrantLock();
        Condition sCondition = sLock.newCondition();
        List<Channel> channels = new ArrayList<>();

//      for (Pair<Channel, Closure> rule : rules) {
//          channels.add(rule.getValue1());
//      }
        for (Iterator<Pair<Channel, Object>> iterator = rules.iterator(); iterator.hasNext(); ) {
            Pair<Channel, Object> rule = iterator.next();
            channels.add(rule.getKey());
        }

        Collections.sort(channels);
//      Pair<Object, Closure> result = null;
        Pair<Object, Object> result = null;

        lockAllChannels(channels);
        sLock.lock();
        try {
            while (result == null) {
                result = pollChannels(rules);

                if (result == null) {
                    addConditions(channels, sLock, sCondition);
                    unlockAllChannels(channels);
                    sCondition.await();
                    lockAllChannels(channels);
                }
            }
        }
        finally {
            unlockAllChannels(channels);
            sLock.unlock();
        }

        return result;
    }

    private void lockAllChannels(List<Channel> channels) {
        for (Iterator<Channel> iterator = channels.iterator(); iterator.hasNext(); ) {
            Channel chan = iterator.next();
            chan.lock();
        }
    }

    private void unlockAllChannels(List<Channel> channels) {
        for (Iterator<Channel> iterator = channels.iterator(); iterator.hasNext(); ) {
            Channel chan = iterator.next();
            chan.unlock();
        }
    }

    private void addConditions(List<Channel> channels, Lock lock, Condition condition) {
        for (Iterator<Channel> iterator = channels.iterator(); iterator.hasNext(); ) {
            Channel chan = iterator.next();
            chan.addCondition(lock, condition);
        }
    }

//  private Pair<Object, Closure> pollChannels(List<Pair<Channel, Closure>> rules) {
    private Pair<Object, Object> pollChannels(List<Pair<Channel, Object>> rules) {
//      Pair<Object, Closure> result = null;
        Pair<Object, Object> result = null;

//      for (Iterator<Pair<Channel, Closure>> iterator = rules.iterator(); iterator.hasNext(); ) {
//          Pair<Channel, Closure> rule = iterator.next();
        for (Iterator<Pair<Channel, Object>> iterator = rules.iterator(); iterator.hasNext(); ) {
            Pair<Channel, Object> rule = iterator.next();
            Channel chan = rule.getKey();
//          Closure clo = rule.getValue();
            Object clo = rule.getValue();

            if (chan.isNonEmpty() && result == null) {
                Object value = chan.poll();
                result = new Pair<>(value, clo);

                chan.signalGetters();
            }
        }

        return result;
    }
}
