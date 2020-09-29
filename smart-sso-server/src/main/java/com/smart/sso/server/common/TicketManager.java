package com.smart.sso.server.common;

import java.util.Timer;
import java.util.TimerTask;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Joe
 */
public abstract class TicketManager {
    
    protected final Logger logger = LoggerFactory.getLogger(getClass());

    // 有效期，单位为秒，默认5分钟
    protected int timeout = 300;
    // 定时器
    private final Timer timer = new Timer(true);

    /**
     * 可自定义注入过期时间
     * 
     * @param timeout
     */
    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }

    // 每分钟执行一次
    public TicketManager() {
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                verifyExpired();
            }
        }, 60 * 1000, 60 * 1000);
    }

    /**
     * 定时清理过期
     */
    public abstract void verifyExpired();
}
