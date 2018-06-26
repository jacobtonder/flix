package ca.uwaterloo.flix;

import java.util.*;
import java.util.concurrent.locks.*;
import java.util.concurrent.atomic.*;

import javafx.util.Pair;


public class Channel implements Comparable<Channel> {
    private static AtomicInteger counter;
    private Integer id;
    private Queue<Object> queue;
    private Integer capacity;
    private Lock lock;
    private Condition channelGetters;
    private Condition channelPutters;
    private List<Pair<Lock, Condition>> conditions;

    // New Channel
    public Channel(Integer capacity) {
        this.id = counter.incrementAndGet();
        this.queue = new LinkedList<>();
        this.capacity = capacity;
        this.lock = new ReentrantLock();
        this.channelGetters = lock.newCondition();
        this.channelPutters = lock.newCondition();
        this.conditions = new ArrayList<>();
    }

    // Get Channel
    public Object getValue() throws InterruptedException {
        Object value = null;

        this.lock();
        try {
            while (this.isEmpty()) {
                this.awaitPutters();
            }

            value = this.queue.poll();

            if (value != null) {
                this.signalPutters();
            }
        }
        finally {
            this.unlock();
        }

        return value;
    }

    // Put Channel
    public Channel putValue(Object value) throws InterruptedException {
        this.lock();
        try {
            while (isFull()) {
                this.awaitGetters();
            }

            this.offer(value);
            this.signalGetters();

            this.signalAllConditions();
            this.conditions.clear();
        }
        finally {
            this.unlock();
        }

        return this;
    }

    public Object poll() {
        return this.queue.poll();
    }

    public Object offer(Object value) {
        return this.queue.offer(value);
    }

    public Boolean isEmpty() {
        return this.queue.isEmpty();
    }

    public Boolean isNonEmpty() {
        return !this.isEmpty();
    }

    public Boolean isFull() {
        return this.size().equals(capacity);
    }

    public Integer size() {
        return this.queue.size();
    }

    public void lock() {
        this.lock.lock();
    }

    public void unlock() {
        this.lock.unlock();
    }

    public void awaitPutters() throws InterruptedException {
        this.channelGetters.await();
    }

    public void awaitGetters() throws InterruptedException {
        this.channelPutters.await();
    }

    public void signalGetters() {
        this.channelGetters.signalAll();
    }

    public void signalPutters() {
        this.channelPutters.signalAll();
    }

    public void addCondition(Lock lock, Condition cond) {
        this.conditions.add(new Pair<>(lock, cond));
    }

    @Override
    public int compareTo(Channel o) {
        return this.id.compareTo(o.id);
    }

    private void signalAllConditions() {
        for (Pair<Lock, Condition> condition : conditions) {
            Lock sLock = condition.getKey();
            Condition sCond = condition.getValue();

            sLock.lock();
            try {
                sCond.signalAll();
            }
            finally {
                sLock.unlock();
            }
        }
    }
}
