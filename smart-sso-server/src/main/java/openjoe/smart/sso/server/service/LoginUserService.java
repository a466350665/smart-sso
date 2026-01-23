package openjoe.smart.sso.server.service;

import openjoe.smart.sso.server.dto.LoginUserDTO;
import openjoe.smart.stage.core.entity.Page;

import java.util.List;

/**
 * 登录用户服务接口
 * 
 * @author Joe
 */
public interface LoginUserService {

	/**
	 * 查询分页列表
	 * @param account 登录名
	 * @param name 姓名
	 * @return
	 */
	Page<LoginUserDTO> selectPage(String account, String name, Long current, Long size);

	/**
	 * 下线
	 * @param tgtList
	 */
	void logout(List<String> tgtList);
}
