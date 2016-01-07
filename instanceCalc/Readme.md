This calculator estimates the amount of memory, cores, and instances for an organization's analytic needs. The inputs are:

* The number of active sessions
* The memory footprint of the active sessions
* Compute requirements
* HA requirements

The key output metrics are *estimated total memory* and *estimated total cores* for the environment. These estimates are based on some assumptions that may or may not apply to any given scenario. The biggest assumption is the multiplier for session memory. Other assumptions include the CPU penalty for running parallel jobs and the CPU benefit for multithreading. These assumptions are based on user experiences that may or may not match your needs.

After memory and core estimates are calculated, the number of stand alone and high availability instances are calculated for a variety of instance types. You can also input a custom instance type.

This caclulator is designed to give you a rough idea of how many instances you will need to support your organization.