package uk.ac.ceb.como.molhub.action;

import com.opensymphony.xwork2.ActionSupport;

public class LoginAction extends ActionSupport {

	private static final long serialVersionUID = 1L;
	
	private String username;
	private String password;

	public String execute() {

		if (this.username.equals("admin") && this.password.equals("admin")) {
			
			return "success";
		} else {
			
			addActionError(getText("error.login"));
			
			return "error";
		}
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

}