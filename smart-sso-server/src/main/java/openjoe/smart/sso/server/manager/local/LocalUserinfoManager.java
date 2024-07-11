package openjoe.smart.sso.server.manager.local;

import openjoe.smart.sso.base.entity.Result;
import openjoe.smart.sso.base.entity.Userinfo;
import openjoe.smart.sso.server.manager.UserinfoManager;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * 用户服务
 */
@Component
public class LocalUserinfoManager implements UserinfoManager {

    private static List<User> userList;

    static {
        userList = new ArrayList<>();
        userList.add(new User(1, "管理员", "admin", "123456"));
    }

    @Override
    public Result<Userinfo> login(String username, String password) {
        for (User user : userList) {
            if (user.getUsername().equals(username)) {
                if (user.getPassword().equals(password)) {
                    return Result.createSuccess(new Userinfo(user.getId(), user.getUsername()));
                } else {
                    return Result.createError("密码有误");
                }
            }
        }
        return Result.createError("用户不存在");
    }

    static class User {

        /**
         * ID
         */
        private Integer id;
        /**
         * 姓名
         */
        private String name;
        /**
         * 登录名
         */
        private String username;
        /**
         * 密码
         */
        private String password;

        public User() {
            super();
        }

        public User(Integer id, String name, String username, String password) {
            super();
            this.id = id;
            this.name = name;
            this.username = username;
            this.password = password;
        }

        public Integer getId() {
            return id;
        }

        public void setId(Integer id) {
            this.id = id;
        }

        public String getName() {
            return this.name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getUsername() {
            return username;
        }

        public void setUsername(String username) {
            this.username = username;
        }

        public String getPassword() {
            return this.password;
        }

        public void setPassword(String password) {
            this.password = password;
        }
    }
}
