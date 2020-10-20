package com.smart.sso.server.common;

import java.util.Timer;
import java.util.TimerTask;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 带时效工具管理器
 * 
 * @author Joe
 */
public abstract class TimeoutManager {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    // 有效期，单位为秒，默认5分钟
    protected int timeout = 300;
    // 定时器
    private final Timer timer = new Timer(true);

    /**
     * 构造方法启动定时工具，每分钟执行一次
     */
    public TimeoutManager() {
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                verifyExpired();
            }
        }, 60 * 1000, 60 * 1000);
    }
    
    public TimeoutManager(int timeout) {
        this();
        this.timeout = timeout;
    }

    /**
     * 可自定义注入过期时间
     * 
     * @param timeout
     */
    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }

    public int getTimeout() {
        return timeout;
    }

    /**
     * 定时清理
     */
    public abstract void verifyExpired();
}
