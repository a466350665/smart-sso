package com.smart.demo.job;


import org.apache.zookeeper.KeeperException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.smart.mvc.config.ConfigUtils;
import com.smart.mvc.locks.DistributedLock;
import com.smart.mvc.locks.ZookeeperLock;

/**
 * 
 * 定时任务,支持多JVM部署，用分布式锁保证执行唯一性
 * 
 * @author Joe
 */
public class UserJob {

	private final Logger LOGGER = LoggerFactory.getLogger(this.getClass());

	public void execute() throws KeeperException, InterruptedException {
		DistributedLock lock = new ZookeeperLock(ConfigUtils.getProperty("zookeeper.address"), this.getClass()
				.getName());
		if (lock.tryLock()) {
			LOGGER.debug(ConfigUtils.getProperty("dubbo.port") + "执行任务");

			/**
			 * 注：当if(lock.tryLock())内代码执行过快，当前的Lock就会被unlock释放掉，会出现多JVM执行多次的情况，设置1秒的缓冲时间
			 */
			Thread.sleep(1000);
		}
		lock.unlock();
	}
}