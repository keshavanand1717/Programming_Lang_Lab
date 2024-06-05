% Predicate to check if an email ends with a specified domain
ends_with_domain(Email, Domain) :-
    atom_concat(_, Domain, Email).

% Predicate to filter valid emails based on domain
filter_valid_emails([], _, []).
filter_valid_emails([Email|Rest], Domain, ValidEmails) :-
    ends_with_domain(Email, Domain),
    filter_valid_emails(Rest, Domain, ValidEmailsRest),
    ValidEmails = [Email|ValidEmailsRest].
filter_valid_emails([_|Rest], Domain, ValidEmails) :-
    filter_valid_emails(Rest, Domain, ValidEmails).

% Predicate to extract usernames from emails
% It separates the email from '@'
extract_usernames([], []).
extract_usernames([Email|Rest], [Username|Usernames]) :-
    atomic_list_concat([Username, _ | _], '@', Email),
    extract_usernames(Rest, Usernames).

% Predicate to count emails by domain
% Count variable incremets when domain matches the given domain
count_emails_by_domain([], _, 0).
count_emails_by_domain([Email|Rest], Domain, Count) :-
    ends_with_domain(Email, Domain),
    count_emails_by_domain(Rest, Domain, RestCount),
    Count is RestCount + 1.
count_emails_by_domain([_|Rest], Domain, Count) :-
    count_emails_by_domain(Rest, Domain, Count).

% Predicate to generate email addresses from usernames and domain
% It concatenates the username with the domain name
generate_email_addresses([], _, []).
generate_email_addresses([Username|Rest], Domain, [Email|Emails]) :-
    atomic_list_concat([Username, '@', Domain], Email),
    generate_email_addresses(Rest, Domain, Emails).

% Example usage to test various questions:
% Valid emails ending with 'yahoo.com'
% ?- filter_valid_emails(["keshav@gmail.com", "anup@yahoo.com", "sahil@yahoo.com", "abhinav@hotmail.com"], "yahoo.com", ValidEmails).
%
% Extract usernames from emails
% ?- extract_usernames(["anand@example.com", "keshav@gmail.com", "rahul@yahoo.org", "anand.keshav@iitg.ac.in"], Usernames).
%
% Count emails by domain
% ?- count_emails_by_domain(["anup@example.com", "anand.keshav@iitg.ac.org", "abhinav@gmail.com", "sarvesh@example.com", "sachin@example.com"], "example.com", Count).
%
% Generate email addresses with a given domain
% ?- generate_email_addresses(["anand.keshav", "abhinav", "sachin.gautam"], "gmail.com", Emails).
