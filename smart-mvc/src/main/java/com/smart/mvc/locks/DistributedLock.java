package com.smart.mvc.locks;

/**
 * 分布式锁
 * 
 * @author Joe
 */
public abstract class DistributedLock {
	/**
	 * 释放锁
	 */
	public abstract void unlock();

	/**
	 * 尝试获得锁，能获得就立马获得锁，如果不能获得就立马返回
	 */
	public abstract boolean tryLock();

	/**
	 * 尝试获得锁，一直阻塞，直到获得锁为止
	 */
	public abstract void lock();
}
