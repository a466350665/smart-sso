package com.smart.demo.job;

import org.apache.zookeeper.KeeperException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.smart.mvc.config.ConfigUtils;
import com.smart.mvc.locks.DistributedLock;
import com.smart.mvc.locks.ZookeeperLock;

/**
 * 
 * 定时任务,支持多JVM部署，用分布式锁保证执行唯一性
 * 
 * @author Joe
 */
@Component
public class UserJob {

	private final Logger LOGGER = LoggerFactory.getLogger(this.getClass());

	/**
	 * 每隔5秒执行一次
	 */
	@Scheduled(cron = "*/5 * * * * ?")
	public void addUserScore() throws KeeperException, InterruptedException {
		DistributedLock lock = new ZookeeperLock(ConfigUtils.getProperty("zookeeper.address"), this.getClass()
				.getName());
		if (lock.tryLock()) {
			LOGGER.debug(ConfigUtils.getProperty("dubbo.port") + "执行任务");

			/**
			 * 注：当tryLock执行过快，会导致当前内容已执行，当前的Lock会被释放掉，会出现多JVM执行多次的情况 设置1秒的缓冲时间
			 */
			Thread.sleep(1000);
		}
		lock.unlock();
	}
}